# `syncdir.scm`

An [`rclone`][rclone] wrapper for two-way synchronization.

Syncs files in a local and remote directory, saving a time stamp for
each file into a file `.sync-times` in the local directory. If both
the local and the remote file have been modified relatively to the
saved time stamp, `syncdir.scm` will run a merge command.

It's also possible to ignore files with names matching a glob. Globs
are read from file `.sync-ignore` in the local directory. It must be
a list of Scheme strings, such as:
```scheme
("*~" "*.tmp" ".*.sw?")
```

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
