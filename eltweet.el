;;; eltweet.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 GC
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: marzo 15, 2022
;; Modified: marzo 15, 2022
;; Version: 0.0.9
;; Keywords: abbrev hypermedia tweet twitter social blog
;; Homepage: https://github.com/cromo/eltweet
;; Package-Requires: ((emacs "27.1"))
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
(require 'org)
(require 'url)
(require 'dom)
(require 'deferred)

(defun eltweet--html-string-to-text (html)
  "Convert a HTML blockquote in simple text."
  (with-temp-buffer
    (insert html)
    (shr-render-region (point-min) (point-max))
    (buffer-string)))

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

(defun eltweet-quote-as-html (uri)
  "Quote a tweet in your buffer with just the URI."
  (interactive "sEnter url: ")
  (insert (cdr (assq 'html (eltweet--quote-from-uri uri))))
  (message "here is your tweet!"))

(defun eltweet-quote-as-simple-text (uri)
  "Print a tweet in simple text with just the URI."
  (interactive "sEnter URL: ")
  (let ((html (cdr (assq 'html (eltweet--quote-from-uri uri))))
        (begin-position (point)))
    ;; (insert "#+begin_quote\n")
    (insert (eltweet--html-string-to-text html))
    ;; (insert "#+end_quote\n")
    (eltweet--indent begin-position (point))))

(defun eltweet--get-href (link)
  "Extract the href value of a parsed LINK."
  (cdr (car (car (cdr link)))))

(defun eltweet--parse-html-string (html)
  "Parse an HTML string and return the parsed result."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun eltweet-quote-as-org (uri)
  "Quote a tweet in your buffer with just the URI."
  (interactive "sEnter url: ")
  ;; (insert (cdr (assq 'html (eltweet--quote-from-uri uri))))
  (let* ((html (cdr (assq 'html (eltweet--quote-from-uri uri))))
         (html-tree (eltweet--parse-html-string html))
         (begin-position (point))
         (strings (dom-strings html-tree))
         (links (dom-by-tag html-tree 'a)))
    (insert "#+begin_quote\n")
    (insert (with-temp-buffer
              (eltweet--orgwriter strings links)
              (buffer-string)))
    (insert "#+end_quote\n")
    (eltweet--indent begin-position (point)))
  (message "Here is your tweet!"))

(defun eltweet--tagger (str)
  "Link da STR."
  (when (> (length str) 2)
    ;; user tag
    (if (string-match-p "@" str)
        (org-link-make-string (concat "https://twitter.com/" (replace-regexp-in-string "@" "" str)) str)
      ;; hashtag
      (if (string-match-p "#" str)
          (org-link-make-string (concat "https://twitter.com/hashtag/" (replace-regexp-in-string "#" "" str)) str)))))

(defun eltweet--orgwriter (strings links)
  "Read into STRINGS and LINKS of a parsed tweet to write a well-formed Org one."
  (when strings
    ;; (message "Eltweet strings: %s" strings)
    (if (< (length strings) 3)
        (if links
          ;; (message "Eltweet links: %s" links)
          (if (= (length links) 1)
              ;; A single link left;
              (progn
                (if (string-match "\\((@.+)\\)" (car strings))
                  (insert
                   (concat "\n\n"
                           (replace-regexp-in-string "\\((@.+)\\)" (org-link-make-string (eltweet--get-href (car links)) (match-string 0 (car strings))) (car strings))))
                  (message "Eltweet error: didn't find any user id in the signed line."))
                (eltweet--orgwriter (cdr strings) (cdr links)))
            ;; More then a link;
            (progn
              (let* ((link (car links))
                     (link-href (eltweet--get-href link))
                     (link-description (cadr (cdr link))))
                (if (eltweet--tagger link-description)
                    (message "Eltweet, discarding tag link: %s" link-description)
                  (insert (concat "\n" (org-link-make-string link-href link-href)))))
              (eltweet--orgwriter strings (cdr links))))
          ;; The last string;
          (insert (concat (car strings) "\n")))
      ;; More then 2 strings;
      (let* ((body-string (car strings))
             (url-parsed (url-generic-parse-url body-string))
             (type (url-type url-parsed))
             (apex-url (if (not type) (url-generic-parse-url (concat "http://" body-string))))
             (tagged (if (not type) (eltweet--tagger body-string))))
        ;; Is the string a full-fledged URL?
        (if (or (equal type "https")
                (equal type "http"))
            (message "Skipping URL %s" body-string)
          ;; Is it an apex, maybe?
          (if (equal (url-host apex-url) "pic.twitter.com")
                (message "Eltweet: skipping fake URL image %s" body-string)
              ;; Is the string some kind of tag?
              (if tagged
                  (progn
                    (message "Eltweet: tag found %s" body-string)
                    (insert tagged))
                ;; Well, I guess it's a simple string, then.
                ;; (message "Eltweet: well, simple string, then: %s" body-string)
                (insert body-string))))
        (eltweet--orgwriter (cdr strings) links)))))

(defun eltweet-async-quote-as-html (uri)
  "Quote a tweet in your buffer with just the URI, do it async."
  (interactive "sEnter url: ")
  (deferred:$
    (eltweet--deferred-quote-from-uri uri)
    (deferred:nextc it
      (lambda (x)
        (insert (cdr (assq 'html x)))))
    (deferred:nextc it
      (message "here is your tweet!"))))

(defun eltweet--parse-json-buffer (buffer)
  (let ((parsed (with-current-buffer buffer
                  (set-buffer-multibyte t)
                  (goto-char (point-min))
                  (re-search-forward "^$")
                  (json-read))))
    (kill-buffer buffer)
    parsed))

(defun eltweet--quote-from-uri (uri)
  "Get a tweet with just the URI."
  (let* ((uri (concat "https://publish.twitter.com/oembed?url=" (url-hexify-string uri) "&omit_script=true"))
         (parsed (eltweet--parse-json-buffer
                  (let ((buffer (url-retrieve-synchronously uri)))
                    (unless buffer (signal 'file-error (list uri "no data")))
                    (when (fboundp 'url-http--insert-file-helper)
                      (url-http--insert-file-helper buffer uri))
                    buffer))))
    parsed))

(defun eltweet--deferred-quote-from-uri (uri)
  "Get a tweet with just the URI, do it async."
  (let ((uri (concat "https://publish.twitter.com/oembed?url=" (url-hexify-string uri) "&omit_script=true")))
    (deferred:$
      (deferred:url-retrieve uri)
      (deferred:nextc it
        (lambda (buffer)
          (eltweet--parse-json-buffer buffer)))
      (deferred:error it
        (lambda (err)
          (message "%s" err))))))

(provide 'eltweet)
;;; eltweet.el ends here
