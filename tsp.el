;;; tsp.el --- Emacs Frontend for tsp. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nils Grunwald <github.com/ngrunwald>
;; Author: Nils Grunwald
;; URL: https://github.com/ngrunwald/tsp-mode
;; Created: 2021
;; Version: 0.1.0
;; Keywords: shell, async, queue, tsp
;; Package-Requires: ((tablist "20200427.2205") (s "20210616.619") (ts "20210813.1617"))

;; This file is NOT part of GNU Emacs.

;; tsp.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; tsp.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with tsp.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs Frontend for tsp.

;;; Code:
(require 'seq)
(require 's)
(require 'ts)
(require 'tabulated-list)
(require 'tablist)
(require 'subr-x)
(require 'dired)
(require 'dired-aux)
(require 'cl-lib)

(defgroup tsp nil
  "tsp customization group."
  :prefix "tsp-"
  :group 'external)

(defcustom tsp-program "tsp" "Name of the tsp program"
  :type 'string)
(defcustom tsp-tasks-list-buffer-name "*TSP Tasks*" "Name of the tasks list buffer"
  :type 'string)
(defcustom tsp-dired-cp-program "cp" "Name of the copy program"
  :type 'string)
(defcustom tsp-dired-mv-program "mv" "Name of the move program"
  :type 'string)

(defun tsp--parse-raw-command (s)
  (cdr (s-match "\\(?:\\[\\([^]]+\\)\\]\\)?\\(.+\\)" s)))

(defun tsp--find-column-idx-by-name (nam)
  (seq-position tabulated-list-format nam (lambda (a b) (string= (car a) b))))

(defun tsp--field-sorter (a b)
  (let ((idx (tsp--find-column-idx-by-name (car tabulated-list-sort-key))))
    (< (get-text-property 0 'sort (aref (cadr a) idx))
       (get-text-property 0 'sort (aref (cadr b) idx)))))

(defun tsp--process-line-fragments (line)
  (if (s-match "^[0-9]+ +finished " line)
      (seq-let (id state output level times raw-command) (s-split-up-to "\s+" line 5)
        (seq-let (label command) (tsp--parse-raw-command raw-command)
          (seq-let (rtime utime stime) (s-split "/" times)
            (list
             (propertize id
                         'state state
                         'output output
                         'level level
                         'rtime rtime
                         'utime utime
                         'stime stime
                         'label label
                         'command command)
             (vector (propertize id
                                 'sort (string-to-number id))
                     (or label "") state level
                     (propertize (ts-human-format-duration (string-to-number rtime) t)
                                 'source (string-to-number rtime)
                                 'sort (string-to-number rtime))
                     command)))))
    (seq-let (id state output raw-command) (s-split-up-to "\s+" line 3)
      (seq-let (label command) (tsp--parse-raw-command raw-command)
        (list
         (propertize id
                     'state state
                     'output output
                     'label label
                     'command command)
         (vector (propertize id
                             'sort (string-to-number id))
                 (or label "")
                 state
                 ""
                 ""
                 command))))))

(defun tsp--tasks-list ()
  (let* ((out (shell-command-to-string (s-concat tsp-program " -l")))
         (tbl (thread-last
                  (seq-rest (s-lines out))
                (seq-remove 's-blank-str-p)
                (seq-map 'tsp--process-line-fragments))))
    tbl))

(defun tsp--retry-task (command &optional label)
  (shell-command-to-string (format "%s%s %s"
                                   tsp-program
                                   (if (s-blank-str? label) "" (s-concat " -L " label))
                                   command)))

(defun tsp-retry-task ()
  (interactive)
  (let ((ids (seq-map 'car (tablist-get-marked-items))))
    (message "Retrying %d tasks" (length ids))
    (seq-each (lambda (id) (tsp--retry-task (get-text-property 0 'command id)
                                            (get-text-property 0 'label id)))
              ids)
    (tablist-revert)))

(defun tsp-find-output ()
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (find-file (get-text-property 0 'output id))
      (view-mode)
      (when (string= (get-text-property 0 'state id) "running")
        (auto-revert-tail-mode)))))

(defun tsp--revert-tasks-list-when-existing ()
  (when (get-buffer tsp-tasks-list-buffer-name)
    (with-current-buffer tsp-tasks-list-buffer-name
      (tablist-revert))))

(defun tsp-clear-finished-tasks ()
  (interactive)
  (shell-command-to-string (s-concat tsp-program " -c"))
  (tsp--revert-tasks-list-when-existing))

(defun tsp-remove-task ()
  (interactive)
  (let ((ids (seq-map 'car (tablist-get-marked-items))))
    (seq-each (lambda (id) (shell-command-to-string (s-concat tsp-program " -r " id)))
              ids))
  (tsp--revert-tasks-list-when-existing))

(defun tsp--task-info (arg)
  (let ((id (or arg (tabulated-list-get-id))))
    (when id
      (let* ((raw (shell-command-to-string (format "%s -i %s" tsp-program id)))
             (data (thread-last raw
                     (s-lines)
                     (seq-remove 's-blank-str?)
                     (seq-map (lambda (l) (s-split ": " l t)))))
             (pid (when (string= "running" (get-text-property 0 'state id))
                    (shell-command-to-string (format "%s -p %s" tsp-program id))))
             (rtime (get-text-property 0 'rtime id))
             (utime (get-text-property 0 'utime id))
             (output (get-text-property 0 'output id)))
        (when  output
            (push (list "Output file" output) data))
        (when rtime
          (push (list "Real time" (format "%ss" rtime)) data))
        (when utime
          (push (list "User time" (format "%ss" utime)) data))

        (push (list "ID" id) data)
        (when pid
          (push (list "PID" pid) data))
        data))))

(defun tsp-task-info (&optional arg)
  (interactive)
  (let ((id (or arg (tabulated-list-get-id))))
    (when id
      (let* ((data (tsp--task-info id))
             (content (thread-last data
                        (seq-map (lambda (kv) (s-join ": " kv)))
                        (seq-remove 's-blank-str?)
                        (s-join "\n"))))
        (switch-to-buffer "*TSP Info*")
        (erase-buffer)
        (insert content)
        (view-mode)))))

(defun tsp-kill-task-process ()
  (interactive)
  (let ((ids (seq-map 'car (tablist-get-marked-items))))
    (seq-each (lambda (id) (shell-command-to-string (s-concat tsp-program " -k " id)))
              ids))
  (tsp--revert-tasks-list-when-existing))

(defun tsp-make-task-urgent ()
  (interactive)
  (let ((ids (seq-map 'car (tablist-get-marked-items))))
    (seq-each (lambda (id) (when (string= "queued" (get-text-property 0 'state id))
                             (shell-command-to-string (s-concat tsp-program " -u " id))))
              ids))
  (tsp--revert-tasks-list-when-existing))

(defun tsp--dired-copy-cmd (sources target)
  (format "%s %s %s -v%s %s %s" tsp-program "-L tsp-dired-copy" tsp-dired-cp-program
          (if (not dired-recursive-copies) "" "r")
          (s-join " " (seq-map 'shell-quote-argument sources)) (shell-quote-argument target)))

(defun tsp--dired-move-cmd (sources target)
  (format "%s %s %s %s %s" tsp-program "-L tsp-dired-move" tsp-dired-mv-program
          (s-join " " (seq-map 'shell-quote-argument sources)) (shell-quote-argument target)))

;;;###autoload
(defun tsp-dired-do-copy (&optional arg)
  "Replacement for `dired-do-copy' but made async through `tsp'."
  (interactive "P")
  (tsp--dired-do-op "Copy" 'copy 'tsp--dired-copy-cmd arg))

;;;###autoload
(defun tsp-dired-do-rename (&optional arg)
  "Replacement for `dired-do-rename' but made async through `tsp'."
  (interactive "P")
  (tsp--dired-do-op "Rename" 'move 'tsp--dired-move-cmd arg))

(defun tsp--dired-do-op (op-name op-symbol op-fn &optional arg)
  (let* ((fn-list (dired-get-marked-files nil arg nil nil t))
         (rfn-list (mapcar #'dired-make-relative fn-list))
         (dired-one-file	; fluid variable inside dired-create-files
          (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
         (target-dir (dired-dwim-target-directory))
         (default (and dired-one-file
                       (not dired-dwim-target) ; Bug#25609
                       (expand-file-name (file-name-nondirectory (car fn-list))
                                         target-dir)))
         (defaults (dired-dwim-target-defaults fn-list target-dir))
         (target (expand-file-name ; fluid variable inside dired-create-files
                  (minibuffer-with-setup-hook
                      (lambda ()
                        (set (make-local-variable 'minibuffer-default-add-function) nil)
                        (setq minibuffer-default defaults))
                    (dired-mark-read-file-name
                     (format "%s %%s: "
                             op-name)
                     target-dir op-symbol arg rfn-list default))))
         (cmd (funcall op-fn rfn-list target)))
    (message "Enqueued %s in tsp: ID %s" op-name (shell-command-to-string cmd))
    (tsp--revert-tasks-list-when-existing)))

;;;###autoload
(defun tsp-dired-do-shell-command (command &optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "! on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (cl-letf (((symbol-function 'dired-run-shell-command)
             (lambda (cmd)
               (let ((cmds (s-split ";" cmd t)))
                 (seq-each (lambda (c) (shell-command-to-string
                                        (format "%s -L tsp-dired-run %s" tsp-program c)))
                           cmds)
                 (message "Enqueued %d jobs in tsp" (length cmds)))
               ;; Return nil for sake of nconc in dired-bunch-files.
               nil)))
    (dired-do-shell-command (s-replace-regexp " ?;?&;?$" "" command) arg file-list)))

(defun tsp--tasks-list-refresh ()
    (setq tabulated-list-entries (tsp--tasks-list)))

(defvar tsp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'tsp-retry-task)
    (define-key map (kbd "i") 'tsp-task-info)
    (define-key map (kbd "K") 'tsp-kill-task-process)
    (define-key map (kbd "k") 'tsp-remove-task)
    (define-key map (kbd "c") 'tsp-clear-finished-tasks)
    (define-key map (kbd "f") 'tsp-find-output)
    (define-key map (kbd "p") 'tsp-make-task-urgent)
    map)
  "Keymap for `tsp-mode'")

(define-derived-mode tsp-mode tabulated-list-mode "tsp-mode"
  "Major mode for handling tsp."
  (setq tabulated-list-format [("ID" 6 tsp--field-sorter)
                               ("Label" 16 t)
                               ("State" 10 t)
                               ("Exit" 6 t)
                               ("Duration" 10 tsp--field-sorter)
                               ("Command" 0 t)])
  (setq-local tabulated-list-entries 'tsp-tasks-list)
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'tsp--tasks-list-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode)
  (local-set-key [remap tablist-do-kill-lines] 'tsp-kill-task-process)
  (tablist-revert))

;;;###autoload
(defun tsp-list-tasks ()
  "Displays the current tsp job queue."
  (interactive)
  (switch-to-buffer tsp-tasks-list-buffer-name)
  (tsp-mode))

(provide 'tsp)
;;; tsp.el ends here
