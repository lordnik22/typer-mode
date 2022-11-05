;;; typer-mode.el --- Type till you die -*- lexical-binding: t -*-

;; Copyright (C) 2022 lordnik22

;; Author: unknown, lordnik22
;; Version: 1.0.0
;; Keywords: games
;; URL: https://github.com/lordnik22
;; Package-Requires: ((emacs "25"))

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;; Commentary:
;; Provides a program to practise your keyboard typing. It provides a
;; buffer with text that need be typed. As the user starts typing the
;; correct characters, they get removed from the buffer. If the user
;; types the wrong character a new line of text is added. This goes on
;; until the user types all characters correct which results in "Game Won".
;; If the user makes to many succesive errors then it’s "Game Over".
;;; Code:
(defgroup typer nil
  "Practise your keyboard typing speed."
  :prefix "typer-"
  :group 'games)

(defcustom typer-max-lines 20
  "If more than this amount of lines are added to the typer buffer, the game ends."
  :group 'typer
  :type 'number)

(defcustom typer-buffer-name "*Typer*"
  "Name used for Typer buffer."
  :group 'typer
  :type 'string)

(defcustom typer-min-words-per-line 1
  "The minimum number of words per line.
Each line contains a number of words between this and
  ‘typer-max-words-per-line’."
  :group 'typer
  :type 'integer)

(defcustom typer-max-words-per-line 10
  "The minimum number of words per line.
Each line contains a number of words between this and
  ‘typer-min-words-per-line’."
  :group 'typer
  :type 'integer)

(defcustom typer-mode-content nil
  "File-path providing the training-content.
From this content random sentences to retype are chosen to
retype. When nil ‘view-emacs-news’ is used."
  :group 'typer
  :type 'string)

(defmacro typer-do (&rest body)
  "Disable `read-only-mode', evaluate BODY, then enable it again."
  `(progn (read-only-mode 0) ,@body (read-only-mode 1)))

(defvar-local typer-point (point-min))
(defvar-local typer-state :typer-playing)
(defvar-local typer-line-queue '())
(defvar-local typer-animation-timer nil)
(define-derived-mode typer-mode nil "Typer"
  "A game for tytpractising typing speed."
  (read-only-mode)
  (buffer-disable-undo)
  (add-hook 'post-command-hook 'typer-post-command-hook nil :local)
  (add-hook 'kill-buffer-hook 'typer-kill-buffer-hook nil :local)

  (typer-do
   (erase-buffer)
   (insert (typer-random-sentences 10)))
  (switch-to-buffer typer-buffer-name)
  (goto-char (point-min)))

(defvar typer-mode-continous-error nil "Only add a punishment-line on first error.
Any consequential error won’t add punishment-lines. Used by
‘typer-mode-handle-match’ and ‘typer-mode-handle-miss’." )
(defun typer-handle-match ()
  "Action to make if typed character matched."
  (typer-do
   (setq typer-mode-continous-error nil)
   (delete-char 1)
   (if (or (string-blank-p (thing-at-point 'line))
           (string= (string (following-char)) "\n"))
       (progn
         (move-beginning-of-line nil)
         (kill-line))
     (insert " "))
   (setq typer-point (point))))

(defun typer-handle-miss ()
  "Action to make if typed character missed."
  (if typer-mode-continous-error
      (user-error "Stay focused!")
    (typer-add-line)
    (setq typer-mode-continous-error t)))

(defun typer-animate-line-insertion ()
  "Animation which comes with punishment-lines."
  (if typer-line-queue
      (let ((token (pop typer-line-queue)))
	(goto-char (point-max))
	(when (not (string= token "\n"))
	  (goto-char (line-beginning-position)))
	(typer-do (insert token))
	(goto-char typer-point))
    (cancel-timer typer-animation-timer)
    (setq typer-animation-timer nil)))

(defun typer-add-line ()
  "Add line at end of buffer which needs to be typed to win the game."
  (let ((line (typer-random-sentences 1)))
    (switch-to-buffer typer-buffer-name)
    (setq typer-line-queue (append typer-line-queue '("\n") (reverse (split-string line "" t)))))
  (when (not (timerp typer-animation-timer))
    (setq typer-animation-timer (run-at-time nil 0.01 'typer-animate-line-insertion))))

(defun typer-game-won ()
  "The player won the game and put’s the buffer into won-state."
  (typer-do (typer-mode--exit-game-y-or-n :typer-game-won)))

(defun typer-game-over ()
  "The player lose the game and put’s the buffer into lose-state."
  (typer-do (typer-mode--exit-game-y-or-n :typer-game-over)))

(defun typer-mode--exit-game-y-or-n (state)
  "General behavior when the game comes to an end. ‘STATE’ describe the end."
   (setq typer-state state)
   (when (timerp typer-animation-timer) (cancel-timer typer-animation-timer))
   (erase-buffer)
   (insert (cond ((eq state :typer-game-won) "Game Won!")
		 ((eq state :typer-game-over) "Game Over!")))
   (setq cursor-type nil)
   (when (y-or-n-p "Kill buffer?")
     (View-exit-and-edit)
     (kill-buffer typer-buffer-name)))

(defun typer-check-state ()
  "Check weather the game continuous or if maximal lines are reached."
  (let ((line-count (count-lines (point-min) (point-max))))
    (when (>= line-count typer-max-lines) (typer-game-over))
    (when (<= line-count 0) (typer-game-won))))

(defun typer-handle-char (arg)
  "Judge if given ‘ARG’ is a match or a miss."
  (if (equal (make-vector 1 (following-char)) arg)
      (typer-handle-match)
    (typer-handle-miss)))

(defun typer-insert-command ()
  "Handle typed characters to progress in the game."
  (interactive)
  (when (equal (symbol-value typer-state) :typer-playing)
    (typer-handle-char (this-command-keys-vector))
    (typer-check-state)))

(defvar typer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'typer-insert-command)
    map))

(defun typer-post-command-hook ()
  "Reposition cursor to new point-min."
  (goto-char typer-point))

(defun typer-kill-buffer-hook ()
  "Cancel any open timers."
  (when (timerp typer-animation-timer)
    (cancel-timer typer-animation-timer)))

(defun typer-random-between (min max)
  "Generate a number between ‘MIN’ and ‘MAX’."
  (if (<= max min)
      min
    (+ min (random (1+ (- max min))))))

(defun typer-random-words-from-current-buffer (n)
  "Pick ‘N’ words from current buffer."
  (if (< n 1)
      nil
    (goto-char (random (point-max)))
    (backward-word)
    (mark-word)
    (let ((word (downcase (buffer-substring-no-properties (mark) (point)))))
      (cons word (typer-random-words-from-current-buffer (1- n))))))

(defun typer-random-sentences-from-current-buffer (n)
  "Pick ‘N’ sentences from current buffer."
  (if (> n 0)
      (let* ((word-count (typer-random-between typer-min-words-per-line typer-max-words-per-line))
	     (words (typer-random-words-from-current-buffer word-count))
	     (sentence (mapconcat 'identity words " ")))
	(cons sentence (typer-random-sentences-from-current-buffer (1- n))))
    nil))

(defun typer-random-sentences (n)
  "Create a string with ‘N’ random sentences from temp-buffer.
‘typer-mode-content’ defines the content of this temp-buffer."
  (with-temp-buffer
    (cond ((stringp typer-mode-content)
	   (if (file-exists-p typer-mode-content)
	       (insert-file-contents typer-mode-content)
	     (user-error "‘typer-mode-content’ not readable!")))
	  (t (view-emacs-news)))
    (mapconcat 'identity (typer-random-sentences-from-current-buffer n) "\n")))

;;;###autoload
(defun typer ()
  "Start the typer game."
  (interactive)
  (select-window (or (get-buffer-window typer-buffer-name)
                     (selected-window)))
  (with-current-buffer (switch-to-buffer typer-buffer-name)
    (typer-mode)
    (switch-to-buffer typer-buffer-name)))
(provide 'typer-mode)
;;; typer-mode.el ends here
