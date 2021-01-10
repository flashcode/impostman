;;; impostman.el --- Import Postman collections  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Sébastien Helleu <flashcode@flashtux.org>

;; Author: Sébastien Helleu <flashcode@flashtux.org>
;; Maintainer: Sébastien Helleu <flashcode@flashtux.org>
;; Created: 2020-12-24
;; Keywords: tools
;; URL: https://github.com/flashcode/impostman
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; Impostman is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Impostman is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Impostman.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Import Postman collections to use them with HTTP clients: verb, restclient
;; or your custom output.

;;; Code:

(require 'subr-x)

;; outputs

(defgroup impostman nil
  "Import Postman collections."
  :prefix "impostman-"
  :group 'tools)

(defcustom impostman-auth-basic-as-elisp-code t
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

(defconst impostman-version "0.1.0"
  "Impostman package version")

(defconst impostman-output-verb-alist
  '((init . ignore)
    (header . impostman-output-verb-header)
    (item . impostman-output-verb-item)
    (request . impostman-output-verb-request)
    (footer . impostman-output-verb-footer)
    (end . impostman-output-verb-end))
  "Emacs verb output")

(defconst impostman-output-restclient-alist
  '((init . ignore)
    (header . impostman-output-restclient-header)
    (item . impostman-output-restclient-item)
    (request . impostman-output-restclient-request)
    (footer . impostman-output-restclient-footer)
    (end . impostman-output-restclient-end))
  "Emacs restclient output")

(defconst impostman-outputs-alist
  '(("verb" . impostman-output-verb-alist)
    ("restclient" . impostman-output-restclient-alist))
  "Impostman outputs")

;; utility functions

(defun impostman-format-comment (comment &optional prefix)
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

(defun impostman-add-query-string-items-to-url (url query-string-items)
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

(defun impostman-get-auth-basic-plain (authorization)
  "Get the plain-text \"username:password\" with the value of the
Authorization header, if it is Basic authentication.
For example with the value \"Basic dXNlcm5hbWU6cGFzc3dvcmQ=\",
the function returns \"username:password\".
Return nil if the authentication is not Basic or if the base64 is invalid."
  (save-match-data
    (when (string-match "^Basic \\(.*\\)" authorization)
      (ignore-errors (base64-decode-string (match-string 1 authorization))))))

;; verb output

(defun impostman-output-verb-header (name description)
  "Format the verb header.

NAME is the collection name.
DESCRIPTION is the collection description."
  (concat
   "* " name "  :verb:\n"
   (impostman-format-comment description)))

(defun impostman-output-verb-item (level name description)
  "Format a verb item.

LEVEL is the level.
NAME is the item name.
DESCRIPTION is the item description."
  (concat
   (if (<= level 2) "\n" "")
   (make-string (max level 1) ?*) " " name "\n"
   (impostman-format-comment description)))

(defun impostman-output-verb-request (description method url headers body)
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
        (when (and impostman-auth-basic-as-elisp-code
                   (string= header-name "Authorization"))
          (let ((auth-plain (impostman-get-auth-basic-plain header-value)))
            (when auth-plain
              (setq new-value
                    (concat
                     "Basic "
                     "{{(base64-encode-string (encode-coding-string \""
                     auth-plain "\" 'utf-8) t)}}")))))
        (push (concat header-name ": " new-value) list-headers)))
    (concat
     (impostman-format-comment description)
     (downcase method) " " url "\n"
     (when list-headers
       (concat (string-join (nreverse list-headers) "\n") "\n"))
     (if (string-empty-p body) "" (concat "\n" body "\n")))))

(defun impostman-output-verb-footer (name)
  "Format the verb footer.

NAME is the collection name."
  (concat
   "\n"
   "* End of " name "\n"
   "\n"
   "# Local Variables:\n"
   "# eval: (verb-mode)\n"
   "# End:\n"))

(defun impostman-output-verb-end ()
  "Function evaluated at the end."
  (when (fboundp 'org-mode)
    (org-mode))
  (when (fboundp 'verb-mode)
    (verb-mode)))

;; restclient output

(defun impostman-output-restclient-header (name description)
  "Format the restclient header.

NAME is the collection name.
DESCRIPTION is the collection description."
  (concat
   "# -*- restclient -*-\n"
   "#\n"
   "# " name "\n"
   (impostman-format-comment description)
   "#\n"))

(defun impostman-output-restclient-item (level name description)
  "Format a restclient item.

LEVEL is the level.
NAME is the item name.
DESCRIPTION is the item description."
  (concat
   (if (<= level 2) "\n" "")
   (make-string (max level 1) ?#) " " name "\n"
   (impostman-format-comment description)))

(defun impostman-output-restclient-request (description method url headers body)
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
        (when (and impostman-auth-basic-as-elisp-code
                   (string= header-name "Authorization"))
          (let ((auth-plain (impostman-get-auth-basic-plain header-value)))
            (when auth-plain
              (push (concat
                     ":auth := (format \"Basic %s\" "
                     "(base64-encode-string (encode-coding-string \""
                     auth-plain "\" 'utf-8) t))")
                    list-variables)
              (setq new-value ":auth"))))
        (push (concat header-name ": " new-value) list-headers)))
    (concat
     (impostman-format-comment description)
     (when list-variables
       (concat (string-join (nreverse list-variables) "\n") "\n"))
     method " " url "\n"
     (when list-headers
       (concat (string-join (nreverse list-headers) "\n") "\n"))
     (if (string-empty-p body) "" (concat body "\n")))))

(defun impostman-output-restclient-footer (name)
  "Format the restclient footer.

NAME is the collection name."
  (concat
   "\n"
   "# End of " name "\n"))

(defun impostman-output-restclient-end ()
  "Function evaluated at the end."
  (when (fboundp 'restclient-mode)
    (restclient-mode)))

;; build of headers and query-string

(defun impostman--build-auth-headers (auth)
  "Return an alist with headers, based on the `auth' JSON item.

AUTH is a hash table."
  (let* ((headers)
         (auth (or auth (make-hash-table :test 'equal)))
         (auth-type (gethash "type" auth "")))
    (cond ((string= auth-type "basic")
           (let ((basic (gethash "basic" auth []))
                 (username)
                 (password))
             (dotimes (i (length basic))
               (let* ((item (elt basic i))
                      (key (gethash "key" item ""))
                      (value (gethash "value" item "")))
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
             (dotimes (i (length apikey))
               (let* ((apikey-item (elt apikey i))
                      (key (gethash "key" apikey-item ""))
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

(defun impostman--build-headers (header)
  "Return an alist with headers, based on the `header' JSON item.

HEADER is a vector with hash tables."
  (let ((headers))
    (dotimes (i (length header))
      (let* ((header-item (elt header i))
             (key (gethash "key" header-item ""))
             (value (gethash "value" header-item "")))
        (push (cons key value) headers)))
    (nreverse headers)))

(defun impostman--build-auth-query-string (auth)
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
            (dotimes (i (length apikey))
              (let* ((apikey-item (elt apikey i))
                     (key (gethash "key" apikey-item ""))
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

(defun impostman--parse-item (items level output-alist)
  "Parse a Postman collection item.

ITEMS is the \"item\" read from collection (vector).
LEVEL is the level.
OUTPUT-ALIST is an alist with the output callbacks."
  (dotimes (i (length items))
    (let* ((item (elt items i))
           (name (gethash "name" item ""))
           (description (gethash "description" item ""))
           (subitem (gethash "item" item))
           (request (gethash "request" item)))
      (insert (funcall (alist-get 'item output-alist)
                       level name description))
      (if subitem
          (impostman--parse-item subitem (1+ level) output-alist)
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
                 (auth-headers (impostman--build-auth-headers auth))
                 (other-headers (impostman--build-headers header))
                 (headers (append auth-headers other-headers)))
            (setq url (impostman-add-query-string-items-to-url
                       url
                       (impostman--build-auth-query-string auth)))
            (insert (funcall
                     (alist-get 'request output-alist)
                     description method url headers body))))))))

(defun impostman--parse-json (collection output-alist)
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
    (impostman--parse-item (gethash "item" collection) 2 output-alist)
    (insert (funcall (alist-get 'footer output-alist) name))
    (goto-char (point-min))
    (funcall (alist-get 'end output-alist))))

;;;###autoload
(defun impostman-parse-file (filename output-alist)
  "Parse a file with a Postman collection.

FILENAME is a filename with a Postman collection.
OUTPUT-ALIST is an alist with the output callbacks."
  (let ((collection))
    (with-temp-buffer
      (insert-file-contents filename)
      (setq collection (json-parse-buffer)))
    (impostman--parse-json collection output-alist)))

;;;###autoload
(defun impostman-parse-string (string output-alist)
  "Parse a string with a Postman collection.

STRING is a Postman collection (JSON format).
OUTPUT-ALIST is an alist with the output callbacks."
  (let ((collection (json-parse-string string)))
    (impostman--parse-json collection output-alist)))

;; Postman collection import

(defun impostman-read-filename ()
  "Read Postman collection filename."
  (interactive)
  (read-file-name "Postman collection file (JSON): "))

(defun impostman-read-output ()
  "Read Postman output type, which must be a key from alist
`impostman-outputs-alist'.

If the alist size is 1, the value is immediately returned without asking
anything."
  (interactive)
  (let* ((default (caar impostman-outputs-alist))
         (prompt (concat "Output format (default is " default "): ")))
    (if (= (length impostman-outputs-alist) 1)
        default
      (completing-read
       prompt impostman-outputs-alist nil t nil nil default))))

(defun impostman--get-output-alist (name)
  "Get output alist with a given NAME. A key with this name must exist in
`impostman-outputs-alist'."
  (let ((output-alist (assoc name impostman-outputs-alist)))
    (if output-alist
        (symbol-value (cdr output-alist))
      (error (format "Output \"%s\" is not supported" name)))))

;;;###autoload
(defun impostman-import-file (&optional filename output-name)
  "Import a file with a Postman collection.

FILENAME is a Postman collection file.
OUTPUT-NAME is a string with the desired output (eg: \"verb\")."
  (interactive)
  (let* ((filename (or filename (impostman-read-filename)))
         (output-name (or output-name (impostman-read-output)))
         (output-alist (impostman--get-output-alist output-name)))
    (impostman-parse-file filename output-alist)))

;;;###autoload
(defun impostman-import-string (string &optional output-name)
  "Import a string with a Postman collection.

STRING is a string with a Postman collection (JSON).
OUTPUT-NAME is a string with the desired output (eg: \"verb\")."
  (interactive)
  (let* ((output-name (or output-name (impostman-read-output)))
         (output-alist (impostman--get-output-alist output-name)))
    (impostman-parse-string string output-alist)))

(provide 'impostman)

;;; impostman.el ends here
