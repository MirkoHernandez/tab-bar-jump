# tab-bar-jump 

tab-bar-jump (abbreviated tbj) is a set of commands for creating and
navigating between tab-bars and buffers (similar to Vim's Harpoon
package). Tab bars belonging to a specific group can be easily created
using a keybinding; once created the same keybinding can be used for
quick navigation. 

This package leverages transient; two commands provide access to most
of the features.

# Features

- Quickly create grouped tab-bars and navigate between them (tbj-jump).
- Navigate between different buffers opened in different tab-bars
  using the same keys (tbj-buffer-jump).
- Automatically assign keys for navigating buffers when they are
  opened (tbj-minor-mode).

# Requirements

Emacs 27.1 (or Transient installed).

# Installation

## Manual
Download the repository to a directory in your load-path.

```console
$ git clone https://github.com/MirkoHernandez/tab-bar-jump
```
Then enable the package.

```emacs-lisp
(require tab-bar-jump)
```

Alternative the package can be installed using M-x `package-install-file`.
M-x `package-initialize` may be required to immediately recognize the
package after installation.

# Usage

In order to use `tbj-jump` the variable  `tbj-groups` must be configured. It should be an alist
consisting of a group and a set of keys for navigating tabs 

> [!NOTE] 
> tab-bar groups are provided primarily to organize each
> tab-bar in the transient.

``` emacs-lisp
(setq tbj-groups 
	'(("prog" ("j" "k" "l" ";" "'" "J" "K" "L" ":" "\""))
	("prog2" ("n" "m" "," "." "/"))
	("documentation" ("a" "s" "d" "f" "g"))
	("terminal" ("z" "x" "c" "v" "b"))
	("notes" ("q" "w" "e" "r" "t" "y"))
	("org" ("u" "i" "o" "p"))))
```

## tbj-jump 
The `tbj-jump` command then creates and opens a transient using a
configuration specified in `tbj-groups`: an alist of group names (a
string describing the group name) and transient keybinding (they
usually involve a single letter).

## tbj-buffer-jump 
The `tbj-buffer-jump` can be used to assign a key to a buffer that can then be used 
 for visiting it. Buffers assigned in different tab-bars can use the same keys.

## tbj-minor-mode
A global minor mode that auto assigns one of the (ideally initial)
letters of a buffer when it is first visited. For example, visiting a
Makefile assigns "M" to that buffer, then `tbj-buffer-jum` and then
"M" will visit that buffer.

## Additional  commands

### tbj-cycle-current-group 

Cycle forward the current group's tabs.

### tbj-cycle-current-group-backward

Cycle backwards the current group's tabs.

### tbj-save-state

A group's current tabs can be saved for later use in the same Emacs
session (this is non persistent). This enables changing the buffers in
some of the tabs and then go back to a previous arrangement.

### tbj-restore-state

Restore a group state's tab-bars.

### tbj-buffer-next

Navigate to the next saved buffer in the current tab-bar.

### tbj-buffer-previous

Navigate to the previous saved buffer in the current tab-bar.
