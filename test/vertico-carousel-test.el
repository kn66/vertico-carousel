;;; vertico-carousel-test.el --- Tests for vertico-carousel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for rotating Vertico candidates.

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'vertico-carousel)
(require 'vertico-indexed)

(defmacro vertico-carousel-test--with-state (candidates index count &rest body)
  "Run BODY with Vertico CANDIDATES, selected INDEX and visible COUNT."
  (declare (indent 3) (debug t))
  `(with-temp-buffer
     (let ((vertico-carousel-mode t)
           (vertico-carousel-cycle t))
       (setq-local vertico--candidates ,candidates
                   vertico--total (length ,candidates)
                   vertico--index ,index
                   vertico--scroll 0
                   vertico--hilit #'identity
                   vertico--metadata '(metadata)
                   vertico-count ,count)
       (cl-letf (((symbol-function #'vertico--window-width)
                  (lambda () 80)))
         ,@body))))

(defun vertico-carousel-test--plain-lines (lines)
  "Return LINES without text properties."
  (mapcar #'substring-no-properties lines))

(defun vertico-carousel-test--face-present-p (face value)
  "Return non-nil if FACE appears in face property VALUE."
  (cond
   ((eq face value) t)
   ((listp value) (memq face value))
   (t nil)))

(ert-deftest vertico-carousel-visible-indices-start-at-selection ()
  (vertico-carousel-test--with-state '("a" "b" "c" "d" "e") 2 3
    (should (equal (vertico-carousel--visible-indices) '(2 3 4)))))

(ert-deftest vertico-carousel-visible-indices-wrap-at-end ()
  (vertico-carousel-test--with-state '("a" "b" "c" "d" "e") 4 3
    (should (equal (vertico-carousel--visible-indices) '(4 0 1)))))

(ert-deftest vertico-carousel-visible-indices-do-not-repeat-candidates ()
  (vertico-carousel-test--with-state '("a" "b") 1 10
    (should (equal (vertico-carousel--visible-indices) '(1 0)))))

(ert-deftest vertico-carousel-visible-indices-start-at-zero-for-prompt ()
  (vertico-carousel-test--with-state '("a" "b" "c") -1 2
    (should (equal (vertico-carousel--visible-indices) '(0 1)))))

(ert-deftest vertico-carousel-visible-indices-clamp-out-of-range-selection ()
  (vertico-carousel-test--with-state '("a" "b" "c") 99 2
    (should (equal (vertico-carousel--visible-indices) '(2 0)))))

(ert-deftest vertico-carousel-visible-indices-handle-zero-count ()
  (vertico-carousel-test--with-state '("a" "b" "c") 1 0
    (should-not (vertico-carousel--visible-indices))))

(ert-deftest vertico-carousel-arrange-handles-empty-candidates ()
  (vertico-carousel-test--with-state nil -1 5
    (should-not (vertico--arrange-candidates))
    (should (= vertico--scroll 0))))

(ert-deftest vertico-carousel-arrange-puts-current-candidate-first ()
  (vertico-carousel-test--with-state '("a" "b" "c" "d") 2 3
    (let ((lines (vertico--arrange-candidates)))
      (should (equal (vertico-carousel-test--plain-lines lines)
                     '("c\n" "d\n" "a\n")))
      (should (vertico-carousel-test--face-present-p
               'vertico-current
               (get-text-property 0 'face (car lines)))))))

(ert-deftest vertico-carousel-arrange-wraps-current-last-candidate ()
  (vertico-carousel-test--with-state '("a" "b" "c" "d") 3 3
    (should (equal (vertico-carousel-test--plain-lines
                    (vertico--arrange-candidates))
                   '("d\n" "a\n" "b\n")))))

(ert-deftest vertico-carousel-arrange-does-not-highlight-prompt-selection ()
  (vertico-carousel-test--with-state '("a" "b") -1 2
    (let ((lines (vertico--arrange-candidates)))
      (should (equal (vertico-carousel-test--plain-lines lines)
                     '("a\n" "b\n")))
      (should-not (vertico-carousel-test--face-present-p
                   'vertico-current
                   (get-text-property 0 'face (car lines)))))))

(ert-deftest vertico-carousel-arrange-omits-leading-group-title ()
  (vertico-carousel-test--with-state '("alpha" "beta" "gamma") 1 3
    (cl-letf (((symbol-function #'vertico--metadata-get)
               (lambda (prop)
                 (and (eq prop 'group-function)
                      (lambda (candidate transform)
                        (if transform
                            candidate
                          (substring candidate 0 1)))))))
      (should (equal (car (vertico-carousel-test--plain-lines
                           (vertico--arrange-candidates)))
                     "beta\n")))))

(ert-deftest vertico-carousel-arrange-shows-group-title-between-groups ()
  (vertico-carousel-test--with-state '("alpha" "beta" "gamma") 0 5
    (let ((vertico-group-format "[%s]"))
      (cl-letf (((symbol-function #'vertico--metadata-get)
                 (lambda (prop)
                   (and (eq prop 'group-function)
                        (lambda (candidate transform)
                          (if transform
                              (upcase candidate)
                            (substring candidate 0 1)))))))
        (should (equal (vertico-carousel-test--plain-lines
                        (vertico--arrange-candidates))
                       '("ALPHA\n" "[b]\n" "BETA\n" "[g]\n" "GAMMA\n")))))))

(ert-deftest vertico-carousel-arrange-hides-group-titles-when-disabled ()
  (vertico-carousel-test--with-state '("alpha" "beta") 0 3
    (let ((vertico-carousel-groups nil)
          (vertico-group-format "[%s]"))
      (cl-letf (((symbol-function #'vertico--metadata-get)
                 (lambda (prop)
                   (and (eq prop 'group-function)
                        (lambda (candidate transform)
                          (if transform
                              (upcase candidate)
                            (substring candidate 0 1)))))))
        (should (equal (vertico-carousel-test--plain-lines
                        (vertico--arrange-candidates))
                       '("alpha\n" "beta\n")))))))

(ert-deftest vertico-carousel-arrange-indexed-mode-numbers-wrapped-rows ()
  (vertico-carousel-test--with-state '("a" "b" "c" "d") 3 3
    (let ((vertico-indexed-mode t))
      (should (equal (vertico-carousel-test--plain-lines
                      (vertico--arrange-candidates))
                     '("0 d\n" "1 a\n" "2 b\n"))))))

(ert-deftest vertico-carousel-arrange-index-map-tracks-displayed-candidates ()
  (vertico-carousel-test--with-state '("alpha" "beta" "gamma") 0 3
    (let ((vertico-indexed-mode t)
          (vertico-group-format "[%s]"))
      (cl-letf (((symbol-function #'vertico--metadata-get)
                 (lambda (prop)
                   (and (eq prop 'group-function)
                        (lambda (candidate transform)
                          (if transform
                              candidate
                            (substring candidate 0 1)))))))
        (should (equal (vertico-carousel-test--plain-lines
                        (vertico--arrange-candidates))
                       '("0 alpha\n" "[b]\n" "1 beta\n")))
        (should (equal vertico-carousel--index-map '(0 1)))))))

(ert-deftest vertico-carousel-prepare-indexed-mode-selects-wrapped-row ()
  (vertico-carousel-test--with-state '("a" "b" "c" "d") 3 3
    (let ((vertico-indexed-mode t)
          (this-command 'vertico-exit)
          (prefix-arg 1))
      (vertico--arrange-candidates)
      (cl-letf (((symbol-function #'vertico--update) #'ignore))
        (vertico--prepare))
      (should (= vertico--index 0))
      (should-not prefix-arg))))

(ert-deftest vertico-carousel-setup-enables-local-cycle ()
  (with-temp-buffer
    (let ((vertico-carousel-mode t)
          (vertico-carousel-cycle t)
          (vertico-cycle nil))
      (vertico--setup)
      (should vertico-cycle))))

(ert-deftest vertico-carousel-setup-respects-disabled-cycle ()
  (with-temp-buffer
    (let ((vertico-carousel-mode t)
          (vertico-carousel-cycle nil)
          (vertico-cycle nil))
      (vertico--setup)
      (should-not vertico-cycle))))

(provide 'vertico-carousel-test)
;;; vertico-carousel-test.el ends here
