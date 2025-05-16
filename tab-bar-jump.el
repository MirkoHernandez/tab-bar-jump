;;; tab-bar-jump.el --- Create and navigate grouped tab bars. -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mirko Hernandez

;; Author: Mirko Hernandez <mirkoh@fastmail.com>
;; Maintainer: Mirko Hernandez <mirkoh@fastmail.com>>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.0.1
;; Keywords: convenience tab-bar
;; URL: https://github.com/MirkoHernandez/tab-bar-jump
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; tab-bar-jump, abbreviated  tbj, provides the `tbj-jump'  command to
;; create and navigate between grouped tab bars in a few keystrokes.


;;; Code:
(require 'cl-lib)
(require 'transient)
(require 'subr-x)

;;;; Variables
(defvar tbj-state-table (make-hash-table :test 'equal))

(defvar tbj-buffer-table (make-hash-table :test 'equal))

(defvar tbj-groups nil
  "Should be an ALIST that describe groups and keybindings.
Each key should be a string that will name a tab-bar group.
Each value is a list of valid transient key bindings (a list of strings).

Example:

\(setq tbj-groups =\'((\"group1\" \=(\"a\" \"s\" \"d\" \"f\" \"h\"))
	(\"group2\" \=(\"j\" \"k\" \"l\" \";\"))))")

(defvar tbj-states nil
  "Should be an list of strings describing grouped tab bars possible states.
Example:

\(setq tbj-states =\'(\"debug\" \"build\" \"doc\" ))")

(defvar tbj-max-columns 6
  "Maximum number of columns for tab-bar-jump transient.")

(defvar tbj-max-rows 1
"Maximum number of rows for tab-bar-jump transient.")

(defvar tbj-force-tab-creation t
  "If non-nil, `tbj-jump' can create a new tab before selecting it.
The tab is created only if there  are less that the expected tabs for the
group.")

;;;; Helpers - Group
(defun tbj-select-group ()
  "Select a group from `tbj-groups' using `completing-read'."
  (if tbj-groups
      (completing-read "Group: " (mapcar 'car tbj-groups))
    (message "%s" (propertize
		   "tbj-groups is empty, it should be defined as an ALIST."
		   'face  'font-lock-warning-face))))

(defun tbj-current-group ()
  "Get the current tab's group."
  (alist-get 'group (tab-bar--current-tab)))

(defun tbj-filter-tabs (group &optional replace-current)
  "Return tab-bars belonging to GROUP.
If REPLACE-CURRENT is non-nil, replace the current tab position with the
complete tab information of the current tab."
  (let ((tabs (seq-filter (lambda (tab)
			    (equal group (alist-get 'group tab)))
			  (tab-bar-tabs))))
    (if replace-current
	(mapcar (lambda (tab)
		  (if (equal (car tab)'current-tab)
		      (tab-bar--tab)
		    tab))
		tabs)
      tabs)))

(defun tbj-new-tab (group)
  "Create a new tab-bar that belongs to GROUP."
  (let* ((tab-bar-new-tab-group group))
    (tab-bar-new-tab)))

;;;; GOTO commands
(defun tbj-goto-tab (group index)
  "Go to the tab that corresponds to INDEX in GROUP."
  (if-let* ((g-tabs (tbj-filter-tabs group))
	    (tab (elt g-tabs index))
	    (tab-group-index (cl-position tab g-tabs :test 'equal))
	    (tab-index (cl-position tab (tab-bar-tabs) :test 'equal)))
      (progn
	(when tab-index
	  ;; NOTE: tab-bar-select-tab starts from 1 not 0.
	  (tab-bar-select-tab (1+ tab-index))
	  (message  "%s::%s" group (1+ tab-group-index))))
    (if (or current-prefix-arg tbj-force-tab-creation)
	(progn
	  (if-let* ((g-tabs (tbj-filter-tabs group))
		    (num-tabs-for-creation (- index (1- (length g-tabs)))))
	      (cl-loop repeat num-tabs-for-creation do
		       (tbj-new-tab group))
	    (tbj-new-tab group)))
      (message  "No tab for position %d" index))))

(defun tbj-create-goto-tab-command (group index)
  "Create a go to tab command.
GROUP is the group of the target tab.
INDEX is the index of that tab in that group."
  `(lambda ()
     (interactive)
     (tbj-goto-tab ,group ,index)))

(defun tbj-create-buffer-key (key)
  (let* ((curr-tab (tab-bar--current-tab))
	 (group (alist-get 'group curr-tab))
	 (g-tabs  (tbj-filter-tabs group))
	 (g-index (cl-position curr-tab g-tabs :test 'equal)))
    (format "%s:%s:%s" key group g-index)))

(defun tbj-create-goto-buffer-command (key)
  "Create  a go  to  buffer command.  KEY  is the  key  used in  the
transient. If there is no buffer  associated with that key (or if
the command is  called with a prefix argument),  then the current
buffer is assigned to that key." 
  `(lambda ()
     (interactive)
     (let* ((buffer-key (tbj-create-buffer-key ,key))
	    (buffer  (gethash buffer-key tbj-buffer-table)))
       (cond  ((and (not current-prefix-arg) buffer (buffer-live-p buffer) )
	       (switch-to-buffer buffer))
	      ((and (not current-prefix-arg) buffer (file-exists-p (buffer-file-name buffer)) )
	       (find-file (buffer-file-name buffer)))
	      (t
	       (puthash buffer-key (current-buffer) tbj-buffer-table))))))

(defun tbj-cycle-current-group (&optional reverse)
  "Cycle forward between the tab-bars of the current tab's group.
REVERSE when non-nil cycles the tab-bars backwards."
  (interactive)
  (if-let* ((curr-tab (tab-bar--current-tab))
	    (group (alist-get 'group curr-tab))
	    (g-tabs  (tbj-filter-tabs group))
	    (g-index (cl-position curr-tab g-tabs :test 'equal))
	    (next-tab (elt g-tabs (mod (if reverse (1- g-index) (1+ g-index)) (length g-tabs))))
	    (next-index (cl-position next-tab (tab-bar-tabs) :test 'equal)))
      (progn
	;; tab-bar-select-tab starts from 1 not 0.
	(tab-bar-select-tab (1+ next-index))
	(message "%s::%s" group g-index))
    (message "%s" "Current tab is not in a group.")))

(defun tbj-cycle-current-group-backward ()
 "Cycle backwards between the tab-bars of the current tab's group."
  (interactive)
  (tbj-cycle-current-group t))

(defun tbj-add ()
  "Create a new tab and add it to a an existing tab group from `tbj-groups'."
  (interactive)
  (let* ((group (tbj-select-group)))
    (tbj-new-tab group)))

(defun tbj-create-transient-group (group keys)
  "Create a transient group for usage with transient.
GROUP is the name of the tab-bar group.
KEYS is a list of strings describing keys."
  (vconcat
   (list  group)
   (remove nil
	   (cl-mapcar (lambda (k)
			(list k (format "%s:%d" group (1+ (seq-position keys k)))
			      (tbj-create-goto-tab-command
			       group  (cl-position k keys))))
		      keys))))

(transient-define-prefix tbj-transient ()
  "tbj-transient is a transient for usage with `tbj-jump'."
  [[]] [[]] [[]])

(defun tbj-redefine-transient ()
  "Redefine `tbj-transient' using tbj-groups."
  (interactive)
  
  (transient-define-prefix tbj-transient ()
    "Redefinition of tbj-transient."
    [[]] [[]] [[]] [[]])
  (let* ((row 0))
    (cl-loop for g in tbj-groups
	     do
	     (let* ((index  (cl-position g tbj-groups  :test 'equal))
		    (column (mod index tbj-max-columns)))
	       (transient-append-suffix 'tbj-transient (list (1- row) column)
		 (tbj-create-transient-group (cl-first g)  (cl-second g)))
	       (when (= column (1- tbj-max-columns))
		 (cl-incf row))))))

(transient-define-prefix tbj-buffer-transient ()
  "tbj-buffer-transient is a transient for usage with `tbj-buffer-jump'."
  [[]] [[]] [[]])

(defun tbj-create-buffer-transient-group (group keys)
  "Create a transient group for usage with transient.
GROUP is the name of the tab-bar group.
KEYS is a list of strings describing keys."
  (vconcat
   (list  group)
   (remove nil
	   (cl-mapcar (lambda (k)
			(list k (format "%s:%s" k (if-let ((buffer-name (buffer-file-name (gethash (tbj-create-buffer-key k) tbj-buffer-table)))) 
							   buffer-name "unassigned"))
			      (tbj-create-goto-buffer-command
			       k )
			      ))
		      keys))))

(defun tbj-redefine-buffer-transient ()
  "Redefine  `tbj-buffer-transient' using  tbj-groups. for  setting jump  to
buffer commands."
  (interactive)
  (transient-define-prefix tbj-buffer-transient ()
    "Redefinition of tbj-buffer-transient."
    [[]] [[]] [[]] [[]])
  (let* ((row 0))
    (cl-loop for g in tbj-groups
	     do
	     (let* ((index  (cl-position g tbj-groups  :test 'equal))
		    (column (mod index tbj-max-columns)))
	       (transient-append-suffix 'tbj-buffer-transient (list (1- row) column)
		 (tbj-create-buffer-transient-group (cl-first g)  (cl-second g)))
	       (when (= column (1- tbj-max-columns))
		 (cl-incf row))))))

(defun tbj-buffer-jump ()
  "Go to a buffer saved in `tbj-buffer-table' using `tbj-buffer-transient'."
  (interactive)
  (if (not tbj-groups)
      (message "%s" (propertize
		     "tbj-groups is empty, it should be defined as an ALIST."
		     'face  'font-lock-warning-face))
    ;; TODO: cache the transient creation.
    (tbj-redefine-buffer-transient)
    (tbj-buffer-transient)))

(defun tbj-jump ()
  "Go to a grouped tab-bar defined in `tbj-groups' using `tbj-transient'."
  (interactive)
  (if (not tbj-groups)
      (message "%s" (propertize
		     "tbj-groups is empty, it should be defined as an ALIST."
		     'face  'font-lock-warning-face))
    ;; TODO: cache the transient creation.
    (tbj-redefine-transient)
    (tbj-transient)))

;;;; State - Helpers
(defun tbj-tab-names (tabs)
 "Return a list of tab names for each tab in TABS."
  (mapcar (lambda (a)
	    (alist-get  'name (cdr a)))
	  tabs))

(defun tbj-get-state-tabs (group name)
  "Return a list of tabs stored in entries to the `tbj-state-table' groups table.
GROUP is the tabs group, NAME is the hash key used to retrieve a set of
stored tabs."
  (when-let ((group-table  (gethash group tbj-state-table)))
    (gethash name
	     group-table)))

;;;; State
(defun tbj-save-group-state (group name)
  "Save the current set of tabs of GROUP.
NAME is the name for the set of tabs that will be saved."
  (let* ((tabs (tbj-filter-tabs group t))
	 (state (gethash group tbj-state-table (make-hash-table :test 'equal))))
    (if tabs
	(progn
	  (message "Tabs: %s"
		   (tbj-tab-names tabs))
	  (puthash name tabs state)
	  (puthash group state tbj-state-table))
      (message "No tabs found for %s in group: %s" name group))))

(defun tbj-restore-group-state (group name)
  "Restore the set of tabs stored in `tbj-state-table'.
GROUP is the tabs group, NAME is the set of group tabs to be restored."
  (let* ((tabs (tbj-get-state-tabs group name)))
    (if tabs
	(progn
	  (tab-bar-close-group-tabs group)
	  (tab-bar-tabs-set (append (tab-bar-tabs) tabs)))
      (message "No tabs found for %s in group: %s" name group))))

(defun tbj-save-state ()
   "Select a group and save its current tabs in `tbj-state-table'."
  (interactive
  (let* ((group (if current-prefix-arg (tbj-select-group)
		  (tbj-current-group)))
	 (name (read-string "State: ")))
    (when group
      (tbj-save-group-state group name)))))


(defun tbj-restore-state ()
 "Select a group and restore a specific set of tabs previously stored."
  (interactive)
  (let* ((group (tbj-select-group))
	 (states (hash-table-keys (gethash group tbj-state-table)))
	 (name (when states (completing-read "State: " states))))
    (when (and group  name)
      (tbj-restore-group-state group name))))

(provide 'tab-bar-jump)
;;; tab-bar-jump.el ends here.
