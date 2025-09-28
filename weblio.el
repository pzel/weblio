;;; weblio.el --- Look up Japanese words on Weblio.jp  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Simon Zelazny

;; Author: Simon Zelazny
;; Version: 0.4.1
;; Package-Requires: ((request "0.3.3") (emacs "25.1"))
;; Keywords: langauges, i18n
;; URL: https://github.com/pzel/weblio
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides two functions, weblio-lookup-region and
;; weblio-lookup-word.  They take the selected (or provided) text and attempt
;; to parse and display its definition from https://weblio.jp.
;; This package in not affiliated with weblio.jp.

;;; Code:
(require 'request)
(require 'dom)

;;;###autoload
(defun weblio-lookup-region (start end)
  "Look up selected region in weblio.jp.
Remove spaces and newlines from the selection before lookup.
Display the results in a fresh buffer, *weblio*

Argument START start of region.
Argument END end of region."
  (interactive "r")
  (weblio-lookup-word (buffer-substring start end)))

;;;###autoload
(defun weblio-lookup-word (word)
  "Look up WORD (with whitespace removed) in the weblio.jp dictionary."
  (interactive "sWeblio lookup: ")
  (let*
      ((clean-word (seq-reduce (lambda (s ws) (string-replace ws "" s))
                               '("\t" " " "\n") word))
       (weblio-url (concat "https://www.weblio.jp/content/" clean-word))
       (result-parser (lambda ()
                        (xml-remove-comments (point) (point-max))
                        (libxml-parse-html-region (point) (point-max))))
       (error-handler (cl-function
                       (lambda (&key symbol-status &allow-other-keys)
                         (error "Failed to look up: %s with error: %s"
                                clean-word symbol-status))))
       (result-buffer-name (concat "*weblio-" clean-word "*"))
       (success-handler
        (cl-function
         (lambda (&key data &allow-other-keys)
           (let*
               ((konkat (lambda(list)
                          (mapcar (lambda(el)
                                    (apply #'concat el))
                                  list)))
                (konkat-strings (lambda(nodes)
                                  (funcall konkat
                                           (mapcar #'dom-strings nodes))))
                (response-body data)
                (midashi (car (dom-by-class response-body "^kijiWrp$")))
                (header (car (funcall konkat-strings
                                      (dom-by-tag midashi 'h2))))
                (paragraphs (funcall konkat-strings
                                     (dom-by-tag midashi 'p)))
                (entries (funcall konkat-strings
                                  (dom-by-class midashi "^kiji$"))))
             (with-output-to-temp-buffer result-buffer-name
               (princ (format "%s\n\n" header))
               (if paragraphs
                   ;; regular entries are made up of <p> blocks. Display them
                   (mapcar (lambda(e)
                             (princ (format "%s\n\n" e)))
                           paragraphs)
                 ;; "jistuyou jiten" entries don't have <p> marks, use
                 ;; bare div.kiji's
                 (mapcar (lambda(e)
                           (princ (format "%s\n\n" e)))
                         entries))
               (princ "_") ;;
               (fit-window-to-buffer)))))))
    (request
      weblio-url
      :parser result-parser
      :error error-handler
      :success success-handler)
    (message  "Looking up %s ..." clean-word)))

(provide 'weblio)
;;; weblio.el ends here
