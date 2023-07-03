;;; eltweet.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 GC
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: marzo 15, 2022
;; Modified: marzo 15, 2022
;; Version: 0.0.5
;; Keywords: abbrev hypermedia tweet twitter social blog
;; Homepage: https://github.com/cromo/eltweet
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'json)

(defun eltweet--quote-from-uri (uri)
  "Get a tweet with just the URI."
  (let* ((uri (concat "https://publish.twitter.com/oembed?url=" (url-hexify-string uri) "&omit_script=true"))
         (parsed (with-current-buffer (let ((buffer (url-retrieve-synchronously uri)))
                                        (unless buffer (signal 'file-error (list uri "no data")))
                                        (when (fboundp 'url-http--insert-file-helper)
                                          (url-http--insert-file-helper buffer uri))
                                        buffer)
                   (set-buffer-multibyte t)
                   (goto-char (point-min))
                   (re-search-forward "^$")
                   (json-read))))
    (cdr (assq 'html parsed))))

(defun eltweet--html-string-to-text (html)
  "Convert a HTML blockquote in simple text."
  (with-temp-buffer
    (insert html)
    (shr-render-region (point-min) (point-max))
    (buffer-string)))

(defun eltweet-quote (uri)
  "Quote a tweet in your buffer with just the URI."
  (interactive "sEnter URL: ")
  (insert (eltweet--quote-from-uri uri))
  (message "Here is your tweet!"))

(defun eltweet--indent (beg end)
  "Indent text from BEG position to END position."
  ;; adapted from evil-indent
  (save-restriction
    (narrow-to-region beg end)
    (if (and (= beg (line-beginning-position))
             (= end (line-beginning-position 2)))
        ;; since some Emacs modes can only indent one line at a time,
        ;; implement "==" as a call to `indent-according-to-mode'
        (indent-according-to-mode)
      (goto-char beg)
      (indent-region beg end))
    ;; Update `beg' and `end'
    (setq beg (point-min)
          end (point-max))
    ;; We also need to tabify or untabify the leading white characters
    (progn
      (let* ((beg-line (line-number-at-pos beg))
             (end-line (line-number-at-pos end))
             (ln beg-line)
             (convert-white (if indent-tabs-mode 'tabify 'untabify)))
        (save-excursion
          (while (<= ln end-line)
            (goto-char (point-min))
            (forward-line (- ln 1))
            (back-to-indentation)
            ;; Whether tab or space should be used is determined by indent-tabs-mode
            (funcall convert-white (line-beginning-position) (point))
            (setq ln (1+ ln))))))))

(defun eltweet-quote-as-simple-text (uri)
  "Print a tweet in simple text with just the URI."
  (interactive "sEnter URL: ")
  (let ((html (eltweet--quote-from-uri uri))
        (begin-position (point)))
    ;; (insert "#+begin_quote\n")
    (insert (eltweet--html-string-to-text html))
    ;; (insert "#+end_quote\n")
    (eltweet--indent begin-position (point))))

(provide 'eltweet)
;;; eltweet.el ends here
