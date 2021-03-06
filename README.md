# syncdir.scm

An [rclone][rclone] wrapper for two-way synchronization.

Syncs files in a local and remote directory, saving a time stamp for
each file into a file `.sync-times` in the local directory. If both
the local and the remote file have been modified relatively to the
saved time stamp, syncdir.scm will run a merge command.

## Config file

There is a config file `$XDG_CONFIG_PATH/syncdir.scm` (if
`$XDG_CONFIG_PATH` isn't set, it defaults to "~/.config"). It's an
association list. The following keys are meaningful:
- `paths`: an association list of named paths. For example, an entry
  might look like this: `(foo "/path/to/local/foo" "remote:foo")`.
  Currently, it's only possible to synchronize named path pairs.
- `ignore-globs`: a list of [globs](#glob-syntax) with which to match
  filenames. All files whose name matches at least one of this globs
  are skipped by syncdir.scm. Example:
  `(ignore-globs "*~" "*.tmp" ".*.sw?")`.
- `merge-cmd`: merge command in the [command syntax](#command-syntax)
  with keys `a`, `b` and `output` as the 2 files being merged and the
  output file respectively. Envioronment variable `SYNCDIR_MERGE`
  overrides this config option, however, it isn't read as a list --
  instead it is put into a template like this:
```scheme
`(,(getenv "SYNCDIR_MERGE") a b output)
```
  If neither is specified, value of `VISUAL` or `EDITOR` (defaulting
  to `"vi"`) is used, with the 3 file names appended (like
  `SYNCDIR_MERGE`). This key can also be [overridden for specific
  files](#glob-overrides)

For example, a config file may look like this (I don't use KDiff3, but
that's what its command line looks like according to documentation):
```scheme
((paths
  (org "~/org" "remote:org"))
 (ignore-globs "*~" ".#*#")
 (merge-cmd
  "kdiff3" a b "-o" output))
```

Leading tilde is supported in filenames (both "~/..." and
"~user/...").

## Path overrides

All keys can be overridden in specific paths (except `paths` itself).
`ignore-globs` list is appended, while `merge-cmd` is replaced
completely. To override/extend a particular key, add a pair `(key .
new-value)` to the path list:
```scheme
(path-name "/local/path" "remote:path" (ignore-globs "more"))
```

## Glob overrides

Some keys (currently only `merge-cmd`) can be overridden for a set of
file names specified as globs. Such override specifications can be
nested. Example:
```scheme
(paths
  (foo "/local/foo" "remote:foo"
    (merge-cmd "merge-in-foo" a b output)
    ("*.org"
      (merge-cmd "merge-org-files-in-foo" a b output)
      ("project-*"
        (merge-cmd "merge-projects-org-files-in-foo" a b output)))))
```

## Glob syntax

Asterisk (`*`, any number of characters), question mark (`?`, any
single character), bracketed expressions (`[...]` or `[!...]` for
complement, ranges supported: `[a-z]`, `-` may appear without special
meaning as first or last character in the set, `]` only as first one),
alternatives (`{A,B,C}`, at least one subexpression should match) and
escaping special characters (for instance, `\[` matches character `[`
itself) are supported in globs.

## Command syntax

Commands are lists of strings and symbols. When the command is run,
symbols (keys) are replaced with their meanings (for instance, in a
merge command, `a`, `b` and `output` are replaced with two merged file
names and the output file name respectively) and the list is executed
as a command. It is *not* passed to the shell but instead executed
'directly': the program is searched on `$PATH` and the arguments are
passed with no modifications (see documentation for Guile's `system*`
procedure).

## Dummy mode

There is a dummy mode in which files aren't copied and merges aren't
run. It can be used to quickly check what's going to be synchronized.
Currently the only way to enable dummy mode is to specify an
environment variable `SYNCDIR_DUMMY`.

## Command line

The following command line options are available:
- `-h`: display a help message.
- `-v`, `-q`, `-Q`: verbose, quiet and silent modes respectively. In
  silent mode, no messages are output to stdout (exceptions are still
  displayed to stderr). In quiet mode, only merge messages are
  displayed (they announce which files are going to be merged). In
  normal mode (default), copies are also announced. In verbose mode,
  exception backtraces are also displayed.
- `-n`: [dummy mode](#dummy-mode).

These options also have long names; run `syncdir.scm -h` to view them.

[rclone]: https://rclone.org/
