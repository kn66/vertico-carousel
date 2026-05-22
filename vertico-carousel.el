;;; vertico-carousel.el --- Rotating top-selection display for Vertico -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (compat "30") (vertico "2.8"))
;; URL: https://github.com/kn66/vertico-carousel
;; Keywords: convenience, completion
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; vertico-carousel is a Vertico display extension.  It keeps the current
;; candidate at the first candidate row and rotates the visible candidate list
;; from that candidate, like `icomplete-vertical-mode'.
;;
;; Usage:
;;   (require 'vertico-carousel)
;;   (vertico-carousel-mode 1)

;;; Code:

(require 'vertico)
(require 'vertico-indexed)
(eval-when-compile (require 'cl-lib))

(defgroup vertico-carousel nil
  "Rotating top-selection display for Vertico."
  :link '(url-link "https://github.com/kn66/vertico-carousel")
  :group 'vertico
  :prefix "vertico-carousel-")

(defcustom vertico-carousel-cycle t
  "When non-nil, enable `vertico-cycle' locally for Vertico sessions.
This makes `vertico-next' and `vertico-previous' wrap at the end and
beginning of the candidate list while `vertico-carousel-mode' is active."
  :type 'boolean
  :group 'vertico-carousel)

(defcustom vertico-carousel-groups t
  "When non-nil, show group titles in the rotated candidate list.
The group title before the current candidate is omitted so that the selected
candidate remains the first candidate row."
  :type 'boolean
  :group 'vertico-carousel)

(defvar-local vertico-carousel--index-map nil
  "Candidate indices in the current carousel display order.")

(defun vertico-carousel--start-index ()
  "Return the first candidate index to display."
  (if (> vertico--total 0)
      (min (max vertico--index 0) (1- vertico--total))
    0))

(defun vertico-carousel--visible-indices ()
  "Return candidate indices visible in carousel order.
Each candidate is included at most once.  If the selected candidate is near
the end of the list, the returned indices wrap to the beginning."
  (when (> vertico--total 0)
    (let ((start (vertico-carousel--start-index))
          (count (min vertico-count vertico--total)))
      (cl-loop for offset below count
               collect (mod (+ start offset) vertico--total)))))

(defun vertico-carousel--window-width ()
  "Return the width available for candidate truncation."
  (or (vertico--window-width) (frame-width)))

(defun vertico-carousel--indexed-mode-p ()
  "Return non-nil if `vertico-indexed-mode' is active."
  (bound-and-true-p vertico-indexed-mode))

(defun vertico-carousel--maybe-group-title (title cand lines)
  "Return a formatted group TITLE for CAND when it fits before LINES.
The title is only emitted if there is room left for both the title and the
candidate that follows it."
  (when (and title (< (1+ (length lines)) vertico-count))
    (vertico--format-group-title title cand)))

(defun vertico-carousel--format-lines (indices)
  "Return formatted candidate lines for INDICES."
  (let* ((raw-candidates
          (mapcar (lambda (index) (nth index vertico--candidates)) indices))
         (affixed-candidates
          (vertico--affixate (mapcar #'vertico--hilit raw-candidates)))
         (group-fun
          (and vertico-carousel-groups
               vertico-group-format
               (vertico--metadata-get 'group-function)))
         (max-width (max 0 (- (vertico-carousel--window-width) 4)))
         (start (car indices))
         title
         first
         lines
         displayed-indices)
    (setq first t)
    (cl-loop for index in indices
             for entry in affixed-candidates
             for display-index from 0
             while (< (length lines) vertico-count)
             do
             (pcase-let ((`(,cand ,prefix ,suffix) entry))
               (when-let* ((new-title (and group-fun (funcall group-fun cand nil))))
                 (unless (or first (equal title new-title))
                   (when-let* ((line (vertico-carousel--maybe-group-title
                                      new-title cand lines)))
                     (push line lines)))
                 (setq title new-title
                       cand (funcall group-fun cand 'transform)))
               (when (string-search "\n" cand)
                 (setq cand (vertico--truncate-multiline cand max-width)))
               (push (if (vertico-carousel--indexed-mode-p)
                         (let ((vertico--index (if (>= vertico--index 0) 0 -1)))
                           (vertico--format-candidate
                            cand prefix suffix display-index 0))
                       (vertico--format-candidate
                        cand prefix suffix index start))
                     lines)
               (push index displayed-indices)
               (setq first nil)))
    (setq vertico-carousel--index-map (nreverse displayed-indices))
    (nreverse lines)))

;;;###autoload
(define-minor-mode vertico-carousel-mode
  "Display Vertico candidates as a rotating list.
The selected candidate is always shown as the first candidate row.  The rows
after it continue from the selected candidate and wrap to the start of the
candidate list."
  :global t
  :group 'vertico-carousel)

(cl-defmethod vertico--setup :after (&context (vertico-carousel-mode (eql t)))
  "Enable carousel session defaults for VERTICO-CAROUSEL-MODE."
  (when vertico-carousel-cycle
    (setq-local vertico-cycle t)))

(cl-defmethod vertico--arrange-candidates
  (&context (vertico-carousel-mode (eql t)))
  "Arrange candidates for VERTICO-CAROUSEL-MODE.
The current selection appears at the first row."
  (let ((indices (vertico-carousel--visible-indices)))
    (setq vertico--scroll (or (car indices) 0))
    (vertico-carousel--format-lines indices)))

(cl-defmethod vertico--prepare
  :before (&context (vertico-carousel-mode (eql t))
                    (vertico-indexed-mode (eql t)))
  "Select carousel candidates by visible index for VERTICO-INDEXED-MODE."
  (when (and prefix-arg (memq this-command vertico-indexed--commands))
    (let* ((row (- (prefix-numeric-value prefix-arg) vertico-indexed-start))
           (index (nth row vertico-carousel--index-map)))
      (if index
          (setq vertico--index index
                prefix-arg nil)
        (minibuffer-message "Out of range")
        (setq this-command #'ignore
              prefix-arg nil)))))

(provide 'vertico-carousel)
;;; vertico-carousel.el ends here
