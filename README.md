# ifctc

A Zig implementation of Google's
[IFTTT (IfThisThenThat) lint](https://fuchsia.dev/fuchsia-src/development/source_code/presubmit_checks).
IFCTC stands for "IF Changes Then Change".

> [!WARNING]
>
> This project isn't actively used yet. Bugs may be lurking.

## Usage

Feed `ifctc` a unified patch file, and it will verify that all files were
modified as specified. Files are read relative to the current working directory.

```sh
$ git diff --unified | ifctc
```

Recognized directives are:

- `LINT.IfChange` (or `LINT.IfChange(label)`), which starts a code block.

- `LINT.ThenChange(paths, ...)`, which reports an error if its contents were
  modified, but not the contents in `paths`.

  Paths may contain a `:label`, in which case the code with that label must be
  modified.

  Paths may be relative, in which case they are relative to the file which has
  the directive. They may also be absolute (i.e. start with `/`), in which case
  they are relative to the directory where `ifctc` is invoked.

### Example

As a concrete example, let's say you have two files with the same constant,
which must be kept in sync:

<!-- LINT.IfChange(example) -->

```py
# constants.py
MIN_VERSION = "0.2.0"
```

```rs
// constants.rs
const MIN_VERSION: &str = "0.2.0";
```

We'll add `LINT` directives to keep them in sync:

```py
# LINT.IfChange
MIN_VERSION = "0.2.0"
# LINT.ThenChange(constants.rs)
```

```rs
// LINT.IfChange
const MIN_VERSION: &str = "0.2.0";
// LINT.ThenChange(constants.py)
```

And run `ifctc` -- everything should be okay:

```sh
$ git diff --unified | ifctc && echo ok
ok
```

Then, if we change one version and forget to update the other:

```diff
diff --git a/constants.rs b/constants.rs
--- a/constants.rs
+++ b/constants.rs
@@ -1,3 +1,3 @@
 // LINT.IfChange
-const MIN_VERSION: &str = "0.2.0";
+const MIN_VERSION: &str = "0.3.0";
 // LINT.ThenChange(constants.py)
```

Then `ifctc` will report an error:

```sh
$ git diff --unified | ifctc
constants.rs:3: file was not modified: constants.py
```

However, if both files are modified instead:

```diff
diff --git a/constants.py b/constants.py
--- a/constants.py
+++ b/constants.py
@@ -1,3 +1,3 @@
 # LINT.IfChange
-MIN_VERSION = "0.2.0"
+MIN_VERSION = "0.3.0"
 # LINT.ThenChange(constants.rs)
diff --git a/constants.rs b/constants.rs
--- a/constants.rs
+++ b/constants.rs
@@ -1,3 +1,3 @@
 // LINT.IfChange
-const MIN_VERSION: &str = "0.2.0";
+const MIN_VERSION: &str = "0.3.0";
 // LINT.ThenChange(constants.py)
```

Then `ifctc` is succeeds:

```sh
$ git diff --unified | ifctc && echo ok
ok
```

<!-- LINT.ThenChange(src/Analysis_test.zig:example) -->

## Testing

Use `zig build test` to run tests.

Because the parsers in this repository have both fast paths and slow paths, they
are typically tested with [`SplitBufferIterator`](src/test_helpers.zig), which
ensures that they behave the same way on different chunk sizes.

Fuzzing is not currently used because it is
[not available on macOS](https://github.com/ziglang/zig/issues/20986).

## Implementation notes

I made this for two reasons:

1. I needed an implementation of IFTTT, which only seems to exist
   [here](https://github.com/ebrevdo/ifttt-lint), but isn't compatible with
   Google's (e.g. it uses "#" for labels, instead of ":").

2. I wanted to make a small project using Zig.

And because it's Zig, I really wanted to play the game and tried to optimize the
tool quite deeply, at the cost of a more complex implementation:

1. Scanning happens in multiple threads at once, without any locking.

   - We achieve this by allocating the map of possibly modified files at the
     start of the program, before we start scanning files. After that, most
     state is thread-local, and consolidated once all threads have finished
     scanning for changes.

1. `LINT.IfChange` and `LINT.ThenChange` directives are found using SIMD.

1. Overall, allocations and copies are kept to a minimum:

   - Lines from the patch that don't contribute to the output are skipped
     without copying them.

   - Nothing from scanned files is copied, except for arguments of `IfChange`
     and `ThenChange` directives.

   - When allocations _are_ needed, that's usually in an arena allocator backed
     by a stack-fallback allocator. Unless you have long paths in your
     `LINT.ThenChange` arguments, no allocation will take place once threads
     have spawned.
