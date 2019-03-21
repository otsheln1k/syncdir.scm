# `syncdir.scm`

An [`rclone`][rclone] wrapper for two-way synchronization.

Syncs files in a local and remote directory, saving a time stamp for
each file into a file `.sync-times` in the local directory. If both
the local and the remote file have been modified relatively to the
saved time stamp, `syncdir.scm` will run a merge command.

There is a config file `$XDG_CONFIG_PATH/syncdir.scm` (if
`$XDG_CONFIG_PATH` isn't set, it defaults to "~/.config"). It's an
association list. The following keys are meaningful:
- `paths`: an association list of preconfigured paths. Running
  `syncdir.scm` with a single argument syncs the respective pair of
  paths. For example, an entry might look like this: `(foo
  "/path/to/local/foo" "remote:foo")`.
- `ignore-globs`: a list of globs with which to match filenames. All
  files whose name matches at least one of this globs is skipped by
  `syncdir.scm`. Example: `("*~" "*.tmp" ".*.sw?")`.

For example, a config file may look like this:
```scheme
((paths
  (org "~/org" "remote:org"))
 (ignore-globs "*~" ".#*#"))
```

Leading tilde is supported in filenames (both "~/..." and
"~user/...").

Asterisk (`*`, any number of characters), question mark (`?`, any
single character), bracketed expressions (`[...]` or `[!...]` for
complement, ranges supported: `[a-z]`, `-` may appear without special
meaning as first or last character in the set, `]` only as first one),
alternatives (`{A,B,C}`, at least one subexpression should match) and
escaping special characters (for instance, `\[` matches character `[`
itself) are supported in globs.

Merge command is specified in `SYNCDIR_MERGE` environment variable.
`$A`, `$B` and `$O` will be replaced in the command with two input
file names and the output file name, respectively. Merge is considered
failed if the command returns zero status and the output file is
readable (it isn't created before running the command). In case
variable `SYNCDIR_MERGE` isn't specified, the value of `EDITOR` or
`VISUAL` (defaulting to "vi") is used to open all three files.
Example (runs Emerge in a new frame of an existing instance of Emacs):
```sh
export SYNCDIR_MERGE="emacsclient -c -e '(emerge-files nil \"\$A\" \"\$B\" \"\$O\" nil'\" '((lambda () (delete-frame))))\" >/dev/null"
```

[rclone]: https://rclone.org/
