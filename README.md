# syncdir.scm

An [rclone][rclone] wrapper for two-way synchronization.

Syncs files in a local and remote directory, saving a time stamp for
each file into a file `.sync-times` in the local directory. If both
the local and the remote file have been modified relatively to the
saved time stamp, `syncdir.scm` will run a merge command.

## Config file

There is a config file `$XDG_CONFIG_PATH/syncdir.scm` (if
`$XDG_CONFIG_PATH` isn't set, it defaults to "~/.config"). It's an
association list. The following keys are meaningful:
- `paths`: an association list of preconfigured paths. Running
  `syncdir.scm` with a single argument syncs the respective pair of
  paths. For example, an entry might look like this: `(foo
  "/path/to/local/foo" "remote:foo")`.
- `ignore-globs`: a list of [globs](#glob-syntax) with which to match
  filenames. All files whose name matches at least one of this globs
  is skipped by `syncdir.scm`. Example: `("*~" "*.tmp" ".*.sw?")`.
- `merge-cmd`: merge command in the [command syntax](#command-syntax)
  with keys `a`, `b` and `output` as the 2 files being merged and the
  output file respectively. Enivoronment variable `SYNCDIR_MERGE`
  overrides this config option. It uses the same syntax as the `cdr`
  part of `merge-cmd` config entry. If neither is specified, value of
  `VISUAL` or `EDITOR` (defaulting to `"vi"`) is used, with the 3 file
  names appended.

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

All keys can be overriden in paths (except `paths`). `ignore-globs`
list is appended, while `merge-cmd` is replaced completely. To
override/extend a particular key, add a pair `(key . new-value)` to
the path list:
```scheme
(path-name "/local/path" "remote:path" (ignore-globs "more"))
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

Commands are text into which variables are inserted on each
invocation. The variables (keys) are usually file names. Resulting
command is then passed to the shell (with Guile's `system` procedure).
Special characters in inserted text are escaped according to the
escaping rules for that command.

Commands may be rather complicated and require large amounts of file
name escaping for shells and other programs which will interpret the
command (e.g. when passing a command to Emacs with `emacsclient -c`).
Thus, rather complex escaping rules might be required to prevent
unexpected results when dealing with unusual file names (including
ones with spaces). Lack of escaping may even make it possible to
inject commands with fancy file names on the remote host.

However, most of the time, relatively simple commands have to be
built, and the defaults are usually enough.

The exact syntax for commands is as follows:
```
([(base-name ... { escaped-chars-string special-rule ...
                   | rule-name })]
 { command-string
   | { command-part | variable } ... })
```
`base-name` and `rule-name` are predefined rule names. The following
predefined rules exist:
- `bourne-shell` (escape spaces and shell special characters)
- `bourne-shell-double-quotes` (escape backslashes and other
  characters special inside double quotes: backslash, dollar sign,
  grave accent and double quote. For compatibility with Bash,
  exclamation mark is also escaped by putting in single quotes instead
  of double quotes)
- `bourne-shell-single-quotes` (only escape single quotes by putting
  them inside double quotes: `' -> '"'"'`: close single quotes, write
  a single quote inside double quotes and reopen single quotes)

`rule-name` can also be:
- `default` (equivalent to `bourne-shell-double-quotes` for string
  commands or to `bourne-shell` for list commands; only valid when no
  base rules are specified)
- `#f` (no escaping)

`escaped-chars-string` is a string of characters which will be escaped
with a backslash. `special-rule`s are pairs of character and string,
where each occurence of character will be substituted with the string.
Special rules take precedence over escaped char set.

The escaping rule is applied on top of rule `base-name` (if
specified).

`command-string` is a command string in which key (variable)
references in form `$key` or `${key}` will be replaced with the
variable's value escaped according to this command's escaping rules.

`command-part`s are strings which will be passed kept as they are.
`variable`s are symbols which will be replaced with their string
values escaped according to the specified rules. Strings in the list
resulting from variable substitution are concatenated and passed to
the shell.

## Dummy mode

There is a dummy mode in which files aren't copied and merges aren't
run. It can be used to quickly check what's going to be synchronized.
Currently the only way to enable dummy mode is to specify an
environment variable `SYNCDIR_DUMMY`.

[rclone]: https://rclone.org/
