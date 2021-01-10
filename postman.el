;;; postman.el --- Export Postman collections to Emacs HTTP clients  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Sébastien Helleu <flashcode@flashtux.org>

;; Author: Sébastien Helleu <flashcode@flashtux.org>
;; Maintainer: Sébastien Helleu <flashcode@flashtux.org>
;; Created: 2020-12-24
;; Keywords: tools
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; Postman-to-emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Postman-to-emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Postman-to-emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Export Postman collections to Emacs HTTP clients: verb and restclient.

;;; Code:

(require 'subr-x)

;; outputs

(defgroup postman nil
  "Export Postman collections to Emacs HTTP clients."
  :prefix "postman-"
  :group 'tools)

(defcustom postman-auth-basic-as-elisp-code t
  "Convert Basic authentication header to elisp code so that the username
and password can be easily edited.

Example with verb:
Authorization: Basic {{(base64-encode-string (encode-coding-string \"username:password\" 'utf-8) t)}}

Example with restclient:
:auth := (format \"Basic %s\" (base64-encode-string (encode-coding-string \"username:password\" 'utf-8) t))
Authorization: :auth

If nil, the username and password are directly encoded in base64:
Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ="
  :type 'boolean)

(defconst postman-version "0.1.0"
  "Postman-to-emacs package version")

(defconst postman-outputs-alist
  '(("verb" . postman-output-verb-alist)
    ("restclient" . postman-output-restclient-alist))
  "The different supported outputs")

(defconst postman-output-verb-alist
  '((init . ignore)
    (header . postman-output-verb-header)
    (item . postman-output-verb-item)
    (request . postman-output-verb-request)
    (footer . postman-output-verb-footer)
    (end . postman-output-verb-end))
  "Emacs verb output")

(defconst postman-output-restclient-alist
  '((init . ignore)
    (header . postman-output-restclient-header)
    (item . postman-output-restclient-item)
    (request . postman-output-restclient-request)
    (footer . postman-output-restclient-footer)
    (end . postman-output-restclient-end))
  "Emacs restclient output")

;; utility functions

(defun postman-format-comment (comment &optional prefix)
  "Format a comment, which can be on multiple lines.

COMMENT is a string, where multiple lines are separated by \"\\n\".
PREFIX is the prefix to add in front of each line (default is \"# \")."
  (let ((comment (or comment ""))
        (prefix (or prefix "# ")))
    (if (string-empty-p comment)
        ""
      (concat
       prefix
       (replace-regexp-in-string "\n" (concat "\n" prefix) comment)
       "\n"))))

(defun postman-add-query-string-items-to-url (url query-string-items)
  "Return the URL with updated query string parameters.

URL is a string.
QUERY-STRING is nil or an alist with query strings to add."
  (dolist (query-string query-string-items)
    (setq url (concat
               url
               (if (string-match-p "\\?" url) "&" "?")
               (car query-string)
               "="
               (cdr query-string))))
  url)

(defun postman-get-auth-basic-plain (authorization)
  "Get the plain-text \"username:password\" with the value of the
Authorization header, if it is Basic authentication.
For example with the value \"Basic dXNlcm5hbWU6cGFzc3dvcmQ=\",
the function returns \"username:password\".
Return nil if the authentication is not Basic or if the base64 is invalid."
  (save-match-data
    (when (string-match "^Basic \\(.*\\)" authorization)
      (ignore-errors (base64-decode-string (match-string 1 authorization))))))

;; verb output

(defun postman-output-verb-header (name description)
  "Format the verb header.

NAME is the collection name.
DESCRIPTION is the collection description."
  (concat
   "* " name "  :verb:\n"
   (postman-format-comment description)))

(defun postman-output-verb-item (level name description)
  "Format a verb item.

LEVEL is the level.
NAME is the item name.
DESCRIPTION is the item description."
  (concat
   (if (<= level 2) "\n" "")
   (make-string (max level 1) ?*) " " name "\n"
   (postman-format-comment description)))

(defun postman-output-verb-request (description method url headers body)
  "Format a verb request.

DESCRIPTION is the request description.
METHOD is the HTTP method.
URL is the URL.
HEADERS is an alist with HTTP headers.
BODY is the request body."
  (let ((list-headers))
    (dolist (header headers)
      (let* ((header-name (car header))
             (header-value (cdr header))
             (new-value header-value))
        (when (and postman-auth-basic-as-elisp-code
                   (string= header-name "Authorization"))
          (let ((auth-plain (postman-get-auth-basic-plain header-value)))
            (when auth-plain
              (setq new-value
                    (concat
                     "Basic "
                     "{{(base64-encode-string (encode-coding-string \""
                     auth-plain "\" 'utf-8) t)}}")))))
        (push (concat header-name ": " new-value) list-headers)))
    (concat
     (postman-format-comment description)
     (downcase method) " " url "\n"
     (when list-headers
       (concat (string-join (nreverse list-headers) "\n") "\n"))
     (if (string-empty-p body) "" (concat "\n" body "\n")))))

(defun postman-output-verb-footer (name)
  "Format the verb footer.

NAME is the collection name."
  (concat
   "\n"
   "* End of " name "\n"
   "\n"
   "# Local Variables:\n"
   "# eval: (verb-mode)\n"
   "# End:\n"))

(defun postman-output-verb-end ()
  "Function evaluated at the end."
  (when (fboundp 'org-mode)
    (org-mode))
  (when (fboundp 'verb-mode)
    (verb-mode)))

;; restclient output

(defun postman-output-restclient-header (name description)
  "Format the restclient header.

NAME is the collection name.
DESCRIPTION is the collection description."
  (concat
   "# -*- restclient -*-\n"
   "#\n"
   "# " name "\n"
   (postman-format-comment description)
   "#\n"))

(defun postman-output-restclient-item (level name description)
  "Format a restclient item.

LEVEL is the level.
NAME is the item name.
DESCRIPTION is the item description."
  (concat
   (if (<= level 2) "\n" "")
   (make-string (max level 1) ?#) " " name "\n"
   (postman-format-comment description)))

(defun postman-output-restclient-request (description method url headers body)
  "Format a restclient request.

DESCRIPTION is the request description.
METHOD is the HTTP method.
URL is the URL.
HEADERS is an alist with HTTP headers.
BODY is the request body."
  (let ((list-variables)
        (list-headers))
    (dolist (header headers)
      (let* ((header-name (car header))
             (header-value (cdr header))
             (new-value header-value))
        (when (and postman-auth-basic-as-elisp-code
                   (string= header-name "Authorization"))
          (let ((auth-plain (postman-get-auth-basic-plain header-value)))
            (when auth-plain
              (push (concat
                     ":auth := (format \"Basic %s\" "
                     "(base64-encode-string (encode-coding-string \""
                     auth-plain "\" 'utf-8) t))")
                    list-variables)
              (setq new-value ":auth"))))
        (push (concat header-name ": " new-value) list-headers)))
    (concat
     (postman-format-comment description)
     (when list-variables
       (concat (string-join (nreverse list-variables) "\n") "\n"))
     method " " url "\n"
     (when list-headers
       (concat (string-join (nreverse list-headers) "\n") "\n"))
     (if (string-empty-p body) "" (concat body "\n")))))

(defun postman-output-restclient-footer (name)
  "Format the restclient footer.

NAME is the collection name."
  (concat
   "\n"
   "# End of " name "\n"))

(defun postman-output-restclient-end ()
  "Function evaluated at the end."
  (when (fboundp 'restclient-mode)
    (restclient-mode)))

;; build of headers and query-string

(defun postman--build-auth-headers (auth)
  "Return an alist with headers, based on the `auth' JSON item.

AUTH is a hash table."
  (let* ((headers)
         (auth (or auth (make-hash-table :test 'equal)))
         (auth-type (gethash "type" auth "")))
    (cond ((string= auth-type "basic")
           (let ((basic (gethash "basic" auth []))
                 (username)
                 (password))
             (seq-doseq (basic-item basic)
               (let ((key (gethash "key" basic-item ""))
                     (value (gethash "value" basic-item "")))
                 (cond ((string= key "username")
                        (setq username value))
                       ((string= key "password")
                        (setq password value)))))
             (when (or username password)
               (let ((auth-base64
                      (base64-encode-string
                       (encode-coding-string
                        (concat username ":" password) 'utf-8))))
                 (push
                  (cons "Authorization" (concat "Basic " auth-base64))
                  headers)))))
          ((string= auth-type "apikey")
           (let ((apikey (gethash "apikey" auth []))
                 (apikey-key)
                 (apikey-value)
                 (apikey-in))
             (seq-doseq (apikey-item apikey)
               (let ((key (gethash "key" apikey-item ""))
                     (value (gethash "value" apikey-item "")))
                 (cond ((string= key "key")
                        (setq apikey-key value))
                       ((string= key "value")
                        (setq apikey-value value))
                       ((string= key "in")
                        (setq apikey-in value)))))
             (when (and (not (string= apikey-in "query"))
                        (or apikey-key apikey-value))
               (push (cons apikey-key apikey-value) headers)))))
    (nreverse headers)))

(defun postman--build-headers (header)
  "Return an alist with headers, based on the `header' JSON item.

HEADER is a vector with hash tables."
  (let ((headers))
    (seq-doseq (header-item header)
      (let ((key (gethash "key" header-item ""))
            (value (gethash "value" header-item "")))
        (push (cons key value) headers)))
    (nreverse headers)))

(defun postman--build-auth-query-string (auth)
  "Return query string parameter to add for authentication as an alist, for
example: (\"key\" . \"value\"), or nil if there's no query string to add.

AUTH is a hash table."
  (let ((query-string-items))
    (when auth
      (let ((auth-type (gethash "type" auth "")))
        (when (string= auth-type "apikey")
          (let ((apikey (gethash "apikey" auth []))
                (apikey-key)
                (apikey-value)
                (apikey-in))
            (seq-doseq (apikey-item apikey)
              (let ((key (gethash "key" apikey-item ""))
                    (value (gethash "value" apikey-item "")))
                (cond ((string= key "key")
                       (setq apikey-key value))
                      ((string= key "value")
                       (setq apikey-value value))
                      ((string= key "in")
                       (setq apikey-in value)))))
            (when (and (string= apikey-in "query")
                       (or apikey-key apikey-value))
              (push (cons apikey-key apikey-value) query-string-items))))))
    (nreverse query-string-items)))

;; JSON parser

(defun postman--parse-item (items level output-alist)
  "Parse a Postman collection item.

ITEMS is the \"item\" read from collection (vector).
LEVEL is the level.
OUTPUT-ALIST is an alist with the output callbacks."
  (seq-doseq (item items)
    (let* ((name (gethash "name" item ""))
           (description (gethash "description" item ""))
           (subitem (gethash "item" item))
           (request (gethash "request" item)))
      (insert (funcall (alist-get 'item output-alist)
                       level name description))
      (if subitem
          (postman--parse-item subitem (1+ level) output-alist)
        (when request
          (let* ((description (gethash "description" request ""))
                 (auth (gethash "auth" request (make-hash-table)))
                 (method (gethash "method" request ""))
                 (header (gethash "header" request []))
                 (body (gethash
                        "raw"
                        (gethash "body" request (make-hash-table)) ""))
                 (url (gethash
                       "raw"
                       (gethash "url" request (make-hash-table)) ""))
                 (auth-headers (postman--build-auth-headers auth))
                 (other-headers (postman--build-headers header))
                 (headers (append auth-headers other-headers)))
            (setq url (postman-add-query-string-items-to-url
                       url
                       (postman--build-auth-query-string auth)))
            (insert (funcall
                     (alist-get 'request output-alist)
                     description method url headers body))))))))

(defun postman--parse-json (collection output-alist)
  "Parse a Postman collection.

COLLECTION is a hash table (parsed JSON).
OUTPUT-ALIST is an alist with the output callbacks."
  (let ((name (gethash "name"
                       (gethash "info" collection (make-hash-table))
                       "unknown"))
        (description (gethash "description"
                              (gethash "info"
                                       collection (make-hash-table)) "")))
    (pop-to-buffer (generate-new-buffer (concat name ".org")))
    (funcall (alist-get 'init output-alist))
    (insert (funcall (alist-get 'header output-alist) name description))
    (postman--parse-item (gethash "item" collection) 2 output-alist)
    (insert (funcall (alist-get 'footer output-alist) name))
    (goto-char (point-min))
    (funcall (alist-get 'end output-alist))))

;;;###autoload
(defun postman-parse-file (filename output-alist)
  "Parse a file with a Postman collection.

FILENAME is a filename with a Postman collection.
OUTPUT-ALIST is an alist with the output callbacks."
  (let ((collection))
    (with-temp-buffer
      (insert-file-contents filename)
      (setq collection (json-parse-buffer)))
    (postman--parse-json collection output-alist)))

;;;###autoload
(defun postman-parse-string (string output-alist)
  "Parse a string with a Postman collection.

STRING is a Postman collection (JSON format).
OUTPUT-ALIST is an alist with the output callbacks."
  (let ((collection (json-parse-string string)))
    (postman--parse-json collection output-alist)))

;; export of Postman collection

(defun postman-read-filename ()
  "Read Postman collection filename."
  (interactive)
  (read-file-name "Postman collection file (JSON): "))

(defun postman-read-output ()
  "Read Postman output type, which must be a key from alist
`postman-outputs-alist'.

If the alist size is 1, the value is immediately returned without asking
anything."
  (interactive)
  (let* ((default (caar postman-outputs-alist))
         (prompt (concat "Output format (default is " default "): ")))
    (if (= (length postman-outputs-alist) 1)
        default
      (completing-read
       prompt postman-outputs-alist nil t nil nil default))))

(defun postman--get-output-alist (name)
  "Get output alist with a given NAME. A key with this name must exist in
`postman-outputs-alist'."
  (let ((output-alist (assoc name postman-outputs-alist)))
    (if output-alist
        (symbol-value (cdr output-alist))
      (error (format "Output \"%s\" is not supported" name)))))

;;;###autoload
(defun postman-export-file (&optional filename output-name)
  "Export a file with a Postman collection.

FILENAME is a Postman collection file.
OUTPUT-NAME is a string with the desired output: \"verb\" or \"restclient\"."
  (interactive)
  (let* ((filename (or filename (postman-read-filename)))
         (output-name (or output-name (postman-read-output)))
         (output-alist (postman--get-output-alist output-name)))
    (postman-parse-file filename output-alist)))

;;;###autoload
(defun postman-export-string (string &optional output-name)
  "Export a string with a Postman collection.

STRING is a string with a Postman collection (JSON).
OUTPUT-NAME is a string with the desired output: \"verb\" or \"restclient\"."
  (interactive)
  (let* ((output-name (or output-name (postman-read-output)))
         (output-alist (postman--get-output-alist output-name)))
    (postman-parse-string string output-alist)))

(provide 'postman)

;;; postman.el ends here
