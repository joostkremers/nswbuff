;;; nswbuff.el --- Quick switching between Emacs buffers.

;; Copyright (C) 1998, 2000, 2001, 2003, 2004 by David Ponce
;; Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
;; Copyright (C) 2017 Joost Kremers

;; Author: David Ponce <david@dponce.com>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 18 May 2017
;; Keywords: extensions convenience
;; Package-Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Revision: $Id$

(defconst nswbuff-version "1.0")

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package provides the commands `nswbuff-switch-to-next-buffer'
;; and `nswbuff-switch-to-previous-buffer' to respectively switch to
;; the next or previous buffer in the buffer list.
;;
;; The `nswbuff-exclude-buffer-regexps' defines a list of regular
;; expressions for excluded buffers.  The default setting excludes
;; buffers whose name begin with a blank character.  To exclude all the
;; internal buffers (that is *scratch*, *Message*, etc...) you could
;; use the following regexps '("^ .*" "^\\*.*\\*").
;;
;; Switching buffers pops-up a status window at the bottom of the
;; selected window.  The status window shows the list of switchable
;; buffers where the switched one is hilighted using
;; `nswbuff-current-buffer-face'.  This window is automatically
;; discarded after any command is executed or after the delay
;; specified by `nswbuff-clear-delay'.
;;
;; The bufferlist is sorted by how recently the buffers were used.  If
;; you prefer a fixed (cyclic) order set `nswbuff-recent-buffers-first'
;; to nil.
;;
;; When the status window disappears because of the clear-delay you
;; still stay in switching mode.  The timeout is only a visual
;; thing.  If you want it to have the same effect as using the buffer,
;; set `nswbuff-clear-delay-ends-switching' to t.
;;
;; The leftmost item in the status window is the active buffer before
;; switching started.  If you want the buffer /after/ switching started
;; there, set `nswbuff-display-original-buffer-first' to nil.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (require 'nswbuff)
;;

;;; History:
;;

;;; Code:
(require 'timer)

;;; Options
;;
(defgroup nswbuff nil
  "Quick switching between Emacs buffers."
  :group 'extensions
  :group 'convenience
  :prefix "nswbuff-")

(defcustom nswbuff-status-window-layout nil
  "Method used to ensure the switched buffer is always visible.
This occurs when the buffer list is larger than the status window
width.  The possible choices are:

- - 'Default' If there is only one window in the frame (ignoring the
              minibuffer one and the status window itself) the status
              window height is adjusted.
              Otherwise horizontal scrolling is used.
- - 'Scroll'  Horizontal scrolling is always used.
- - 'Adjust'  Only adjust the window height."
  :group 'nswbuff
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Scroll"  scroll)
                 (const :tag "Adjust"  adjust)))

(defcustom nswbuff-clear-delay 3
  "Time in seconds to delay before discarding the status window."
  :group 'nswbuff
  :type '(number :tag "seconds"))

(defcustom nswbuff-clear-delay-ends-switching nil
  "Should switching end after the clear-delay expired?
If nil, you stay in switching mode and the selected buffer does not
count as used just because the status window disappeared after the
timeout.  But if you prefer so set it to t."
  :group 'nswbuff
  :type 'boolean)

(defcustom nswbuff-display-original-buffer-first t
  "Should the old buffer be first in the list?
If non-nil, the buffer where switching started will be the leftmost in
the list.  Otherwise it will be the buffer the first command switched
to."
  :group 'nswbuff
  :type 'boolean)

(defcustom nswbuff-recent-buffers-first t
  "Show recent buffers first?
If non-nil the buffer list is sorted by how recently the buffers were
used.  If nil, it is as a cyclic list with fixed order.  Note that
other commands (switch-to-buffer) still change the order."
  :group 'nswbuff
  :type 'boolean)

(defcustom nswbuff-separator ", "
  "String used to separate buffer names in the status line."
  :group 'nswbuff
  :type 'string)

(defcustom nswbuff-header ""
  "Status line header string."
  :group 'nswbuff
  :type 'string)

(defcustom nswbuff-trailer ""
  "Status line trailer string."
  :group 'nswbuff
  :type 'string)

(defcustom nswbuff-window-min-text-height 1
  "Minimum text height of the status window."
  :group 'nswbuff
  :type 'integer)

(defface nswbuff-default-face '((t nil))
  "Default face used for buffer names."
  :group 'nswbuff)

(defface nswbuff-current-buffer-face '((t (:foreground "red" :bold t :underline t)))
  "Face used to highlight the current buffer name."
  :group 'nswbuff)

(defface nswbuff-separator-face '((t (:foreground "blue")))
  "Face used for separators."
  :group 'nswbuff)

(defcustom nswbuff-exclude-buffer-regexps '("^ ")
  "List of regular expressions for excluded buffers.
The default setting excludes buffers whose name begin with a blank
character.  To exclude all the internal buffers (that is *scratch*,
*Message*, etc...) you could use the following regexps:
  (\"^ \" \"^\\*.*\\*\")."
  :group 'nswbuff
  :type '(repeat (regexp :format "%v")))

;;; Internals
;;
(defconst nswbuff-status-buffer-name " *nswbuff*"
  "Name of the working buffer used by nswbuff to display the buffer list.")

(defun nswbuff-include-p (name)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `nswbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote nswbuff-status-buffer-name)
                  nswbuff-exclude-buffer-regexps)))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun nswbuff-buffer-list ()
  "Return the list of switchable buffers.
That is without the ones whose name matches
`nswbuff-exclude-buffer-regexps'."
  (let ((blist (delq nil
                     (mapcar (function
                              (lambda (buf)
                                (and (nswbuff-include-p (buffer-name buf))
                                     buf)))
                             (buffer-list)))))
    (or (memq (current-buffer) blist)
        (setq blist (cons (current-buffer) blist)))
    blist))

(if (fboundp 'count-lines)
    (defalias 'nswbuff-count-lines 'count-lines)
  (defun nswbuff-count-lines (start end)
    "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (if (eq selective-display t)
            (save-match-data
              (let ((done 0))
                (while (re-search-forward "[\n\C-m]" nil t 40)
                  (setq done (+ 40 done)))
                (while (re-search-forward "[\n\C-m]" nil t 1)
                  (setq done (+ 1 done)))
                (goto-char (point-max))
                (if (and (/= start end)
                         (not (bolp)))
                    (1+ done)
                  done)))
          (- (buffer-size) (forward-line (buffer-size))))))))

(defun nswbuff-window-lines ()
  "Return the number of lines in current buffer.
This number may be greater than the number of actual lines in the
buffer if any wrap on the display due to their length."
  (nswbuff-count-lines (point-min) (point-max)))

(defun nswbuff-adjust-window (&optional text-height)
  "Adjust window height to fit its buffer contents.
If optional TEXT-HEIGHT is non-nil adjust window height to this
value."
  (setq text-height (max nswbuff-window-min-text-height
                         (or text-height
                             (nswbuff-window-lines))))
  (if (fboundp 'set-window-text-height)
      (set-window-text-height nil text-height)
    (let ((height (window-height))
          (lines  (+ 2 text-height)))
      (enlarge-window (- lines height))))
  (goto-char (point-min)))

;; Used to prevent discarding the status window on some mouse event.
(defalias 'nswbuff-ignore 'ignore)

(defun nswbuff-scroll-window (position)
  "Adjust horizontal scrolling to ensure that POSITION is visible."
  (setq truncate-lines t)
  (let ((auto-hscroll-mode t))
    (goto-char position)))

;; Use mouse-1, mouse-3 on mode line buffer identification to
;; respectively switch to previous or next buffer.  And mouse-2 to
;; kill the current buffer.
(let ((map mode-line-buffer-identification-keymap))
  (define-key map [mode-line mouse-1]
    'nswbuff-switch-to-previous-buffer)
  (define-key map [mode-line drag-mouse-1]
    'nswbuff-ignore)
  (define-key map [mode-line down-mouse-1]
    'nswbuff-ignore)
  (define-key map [mode-line mouse-2]
    'nswbuff-kill-this-buffer)
  (define-key map [mode-line mouse-3]
    'nswbuff-switch-to-next-buffer))

(defun nswbuff-one-window-p (window)
  "Return non-nil if there is only one window in this frame.
Ignore WINDOW and the minibuffer window."
  (let ((count 0))
    (walk-windows #'(lambda (w)
                      (or (eq w window) (setq count (1+ count)))))
    (= count 1)))

(defvar nswbuff-buffer-list-holder nil
  "Hold the current displayed buffer list.")

(defun nswbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let ((blist  nswbuff-buffer-list-holder)
        (head   (or nswbuff-header    "" ))
        (separ  (or nswbuff-separator " "))
        (trail  (or nswbuff-trailer   "" ))
        (width  (window-width window))
        (lines  0)
        (adjust (or (eq nswbuff-status-window-layout 'adjust)
                    (nswbuff-one-window-p window)))
        start end buffer bname fillr)
    (save-selected-window
      (select-window window)
      (setq header-line-format nil) ;; Hide Emacs 21 header line.
      (erase-buffer)
      (setq start (point))
      (insert head)
      (if (> (point) start)
          (set-text-properties
           start (point) '(face nswbuff-separator-face)))
      (while blist
        (setq buffer (car blist)
              blist  (cdr blist))
        (when (buffer-live-p buffer)
          (setq bname (buffer-name buffer)
                start (point)
                fillr (if blist separ trail))
          (when (and adjust
                     (> (- (+ start (length bname) (length fillr))
                           (* lines width))
                        width))
            (newline)
            (setq start (point)
                  lines (1+ lines)))
          (insert bname)
          (cond
           ((string-equal bname bcurr)
            (setq end (point))
            (set-text-properties
             start end '(face nswbuff-current-buffer-face)))
           (t
            (set-text-properties
             start (point) '(face nswbuff-default-face))))
          (setq start (point))
          (insert fillr)
          (if (> (point) start)
              (set-text-properties
               start (point) '(face nswbuff-separator-face)))))
      (if adjust
          (nswbuff-adjust-window)
        (nswbuff-adjust-window 1)
        (nswbuff-scroll-window end)))))

(defvar nswbuff-timer nil
  "Timer used to discard the status window.")

(defun nswbuff-show-status-window ()
  "Pop-up a status window at the bottom of the selected window.
The status window shows the list of switchable buffers where the
switched one is hilighted using `nswbuff-current-buffer-face'.  It is
automatically discarded after any command is executed or after the
delay specified by `nswbuff-clear-delay'."
  (nswbuff-start-switching)
  (if nswbuff-buffer-list-holder
      (let ((bcurr (buffer-name))
            (window-min-height 1)
            cursor-in-non-selected-windows)
        (with-current-buffer (get-buffer-create nswbuff-status-buffer-name)
          (let ((w (or (get-buffer-window nswbuff-status-buffer-name)
                       (split-window-vertically -2))))
            (set-window-buffer w (current-buffer))
            (nswbuff-layout-status-line w bcurr)
            (add-hook 'pre-command-hook 'nswbuff-pre-command-hook)
            (if (timerp nswbuff-timer)
                (cancel-timer nswbuff-timer))
            (setq nswbuff-timer (run-with-timer
                                nswbuff-clear-delay nil
                                #'nswbuff-clear-delay-hook)))))
    (message "No buffers eligible for switching.")))

(defun nswbuff-discard-status-window ()
  "Discard the status window."
  (let ((w (get-buffer-window nswbuff-status-buffer-name))
        (b (get-buffer nswbuff-status-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))

(defun nswbuff-start-switching ()
  "Make sure nswbuff-buffer-list-holder is set before proceeding."
  (or nswbuff-buffer-list-holder
      (setq nswbuff-buffer-list-holder (nswbuff-buffer-list))))

(defun nswbuff-end-switching ()
  "Called when the buffer finally is choosen."
  (if nswbuff-recent-buffers-first
      (let ((bcurr (current-buffer))
            (l (nreverse nswbuff-buffer-list-holder)))
        (while l
          (switch-to-buffer (car l))
          (setq l (cdr l)))
        (switch-to-buffer bcurr)))
  (setq nswbuff-buffer-list-holder nil))

(defun nswbuff-clear-delay-hook ()
  (nswbuff-discard-status-window)
  (and nswbuff-clear-delay-ends-switching (nswbuff-end-switching)))

(defun nswbuff-pre-command-hook ()
  "Track successive calls to switch commands.
Run as a `pre-command-hook'."
  (if (memq this-command '(nswbuff-switch-to-previous-buffer
                           nswbuff-switch-to-next-buffer
                           nswbuff-kill-this-buffer
                           nswbuff-ignore))
      nil
    (nswbuff-discard-status-window)
    (nswbuff-end-switching))
  (if (timerp nswbuff-timer)
      (cancel-timer nswbuff-timer))
  (setq nswbuff-timer nil)
  (remove-hook 'pre-command-hook 'nswbuff-pre-command-hook))

(defun nswbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((l (nswbuff-buffer-list)))
    (and l (switch-to-buffer (nth (1- (length l)) l)))))

(defun nswbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((l (nreverse (nswbuff-buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l)))))

;;; Commands
;;

;;;###autoload
(defun nswbuff-switch-to-previous-buffer ()
  "Switch to the previous buffer in the buffer list."
  (interactive)
  (and nswbuff-display-original-buffer-first (nswbuff-start-switching))
  (nswbuff-previous-buffer)
  (nswbuff-show-status-window))

;;;###autoload
(defun nswbuff-switch-to-next-buffer ()
  "Switch to the next buffer in the buffer list."
  (interactive)
  (and nswbuff-display-original-buffer-first (nswbuff-start-switching))
  (nswbuff-next-buffer)
  (nswbuff-show-status-window))

;;;###autoload
(defun nswbuff-kill-this-buffer ()
  "Kill the current buffer.
And update the status window if showing."
  (interactive)
  (kill-buffer (current-buffer))
  (and (get-buffer-window nswbuff-status-buffer-name)
       (nswbuff-show-status-window)))

(provide 'nswbuff)

;;; nswbuff.el ends here
