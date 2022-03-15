;;; eltweet.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 GC
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: marzo 15, 2022
;; Modified: marzo 15, 2022
;; Version: 0.0.1
;; Keywords: abbrev hypermedia tweet twitter social blog
;; Homepage: https://github.com/cromo/eltweet
;; Package-Requires: ((emacs "24.3"))
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

;;;###autoload
(defun eltweet-quote (uri)
  "Get a tweet blockquote via the Twitter APIs with just the URL (as URI)."
  (interactive "sEnter URL: ")
  (setq uri (concat "https://publish.twitter.com/oembed?url=" (url-hexify-string uri) "&omit_script=true"))
  (let ((parsed (with-current-buffer (let ((buffer (url-retrieve-synchronously uri)))
                                       (unless buffer (signal 'file-error (list uri "no data")))
                                       (when (fboundp 'url-http--insert-file-helper)
                                         (url-http--insert-file-helper buffer uri))
                                       buffer)
                  (goto-char (point-min))
                  (re-search-forward "^$")
                  (json-read))))
    (insert (cdr (assq 'html parsed))))
  (message "Here is your tweet!"))

(provide 'eltweet)
;;; eltweet.el ends here
