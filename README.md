# tab-bar-jump 

tab-bar-jump (abbreviated tbj) is a set of commands for creating and
navigating grouped tab-bars. Tab bars belonging to a specific group
can be easily created using a keybinding, once created the same
keybinding can be used for quick navigation. This approach allows the
management of many tab-bars (up to dozens) in a very convenient way.

# Requirements

Emacs 27.1 or Transient installed.

# Installation

## Manual
Download the repository to a directory in your load-path.

```console
$ git clone https://github.com/MirkoHernandez/tab-bar-jump
```

```emacs-lisp
(require tab-bar-jump)
```

# Usage

The `tbj-jump` command creates and opens a transient using a
configuration specified in `tbj-groups`: an alist of group names (a
string) and transient keybinding.

``` emacs-lisp
(setq tbj-groups 
   '(("prog" ("j" "k" "l" ";" "'" "J" "K" "L" ":" "\""))
	("prog2" ("n" "m" "," "." "/"))
	("prog3" ("a" "s" "d" "f" "g"))
	("prog4" ("z" "x" "c" "v" "b"))
	("notes" ("q" "w" "e" "r" "t" "y"))
	("org" ("u" "i" "o" "p"))))
```

## Additional  commands

### tbj-cycle-current-group 

Cycle forward the current group's tabs.

### tbj-cycle-current-group-backward

Cycle backwards the current group's tabs.

### tbj-save-state

The a group tabs can be saved for later use in the same Emacs session
(this is non persistent).

### tbj-restore-state

Restore a group state's tab-bars.

