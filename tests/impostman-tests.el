;;; impostman-tests.el --- Tests on impostman.el  -*- lexical-binding: t -*-

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

;; Tests on impostman.el

;;; Code:

(require 'ert)
(require 'impostman)

(ert-deftest impostman-format-comment ()
  "Test the format of comment."
  ;; without prefix: default is "# "
  (should (equal (impostman-format-comment nil)
                 ""))
  (should (equal (impostman-format-comment "")
                 ""))
  (should (equal (impostman-format-comment "Test")
                 "# Test\n"))
  (should (equal (impostman-format-comment "Test\nLine 2")
                 (concat
                  "# Test\n"
                  "# Line 2\n")))
  (should (equal (impostman-format-comment "Test\nLine 2\nLine 3")
                 (concat
                  "# Test\n"
                  "# Line 2\n"
                  "# Line 3\n")))

  ;; with a prefix
  (should (equal (impostman-format-comment "" ";; ")
                 ""))
  (should (equal (impostman-format-comment "Test" ";; ")
                 ";; Test\n"))
  (should (equal (impostman-format-comment "Test\nLine 2" ";; ")
                 (concat
                  ";; Test\n"
                  ";; Line 2\n")))
  (should (equal (impostman-format-comment "Test\nLine 2\nLine 3" ";; ")
                 (concat
                  ";; Test\n"
                  ";; Line 2\n"
                  ";; Line 3\n"))))

(ert-deftest impostman-add-query-string-items-to-url ()
  "Test add of query-string items to an URL."
  (should (equal (impostman-add-query-string-items-to-url
                  "https://example.com" nil)
                 "https://example.com"))
  (should (equal (impostman-add-query-string-items-to-url
                  "https://example.com" '(("apikey" . "1a2b3c4d")))
                 "https://example.com?apikey=1a2b3c4d"))
  (should (equal (impostman-add-query-string-items-to-url
                  "https://example.com?a=1" '(("apikey" . "1a2b3c4d")))
                 "https://example.com?a=1&apikey=1a2b3c4d")))

(ert-deftest impostman-get-auth-basic-plain ()
  "Test the base64 decoding of Authorization Basic header."
  (should (equal (impostman-get-auth-basic-plain "")
                 nil))
  (should (equal (impostman-get-auth-basic-plain "test")
                 nil))
  (should (equal (impostman-get-auth-basic-plain "Basic a")
                 nil))
  (should (equal (impostman-get-auth-basic-plain
                  "Basic dXNlcm5hbWU6cGFzc3dvcmQ=")
                 "username:password")))

(ert-deftest impostman-output-verb-header ()
  "Test the output of verb header."
  (should (equal (impostman-output-verb-header "test" "Description\nLine 2")
                 (concat
                  "* test  :verb:\n"
                  "# Description\n"
                  "# Line 2\n"))))

(ert-deftest impostman-output-verb-item ()
  "Test the output of verb item."
  (should (equal (impostman-output-verb-item 0 "test" "")
                 (concat
                  "\n"
                  "* test\n")))
  (should (equal (impostman-output-verb-item 1 "test" "")
                 (concat
                  "\n"
                  "* test\n")))
  (should (equal (impostman-output-verb-item 2 "test" "")
                 (concat
                  "\n"
                  "** test\n")))
  (should (equal (impostman-output-verb-item 3 "test" "")
                 "*** test\n"))
  (should (equal (impostman-output-verb-item 0 "test" "Description\nend.")
                 (concat
                  "\n"
                  "* test\n"
                  "# Description\n"
                  "# end.\n")))
  (should (equal (impostman-output-verb-item 1 "test" "Description\nend.")
                 (concat
                  "\n"
                  "* test\n"
                  "# Description\n"
                  "# end.\n")))
  (should (equal (impostman-output-verb-item 2 "test" "Description\nend.")
                 (concat
                  "\n"
                  "** test\n"
                  "# Description\n"
                  "# end.\n")))
  (should (equal (impostman-output-verb-item 3 "test" "Description\nend.")
                 (concat
                  "*** test\n"
                  "# Description\n"
                  "# end.\n"))))

(ert-deftest impostman-output-verb-request ()
  "Test the output of verb request."
  (should (equal (impostman-output-verb-request
                  ""
                  "GET"
                  "users"
                  nil
                  "")
                 "get users\n"))
  (should (equal (impostman-output-verb-request
                  ""
                  "GET"
                  "users"
                  nil
                  "{\"login\": \"admin\"}")
                 (concat
                  "get users\n"
                  "\n{\"login\": \"admin\"}\n")))
  (should (equal (impostman-output-verb-request
                  ""
                  "GET"
                  "users"
                  '(("Authorization" . "token"))
                  "{\"login\": \"admin\"}")
                 (concat
                  "get users\n"
                  "Authorization: token\n"
                  "\n{\"login\": \"admin\"}\n")))
  (should (equal (impostman-output-verb-request
                  "Description\nend."
                  "GET"
                  "users"
                  '(("Authorization" . "token") ("header" . "value"))
                  "{\"login\": \"admin\"}")
                 (concat
                  "# Description\n"
                  "# end.\n"
                  "get users\n"
                  "Authorization: token\n"
                  "header: value\n"
                  "\n{\"login\": \"admin\"}\n")))
  (should (equal (impostman-output-verb-request
                  "Description\nend."
                  "GET"
                  "users"
                  '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                    ("header" . "value"))
                  "{\"login\": \"admin\"}")
                 (concat
                  "# Description\n"
                  "# end.\n"
                  "get users\n"
                  "Authorization: Basic {{(base64-encode-string "
                  "(encode-coding-string \"user1:Password!\" 'utf-8) t)}}\n"
                  "header: value\n"
                  "\n{\"login\": \"admin\"}\n")))
  (let ((impostman-auth-basic-as-elisp-code))
    (should (equal (impostman-output-verb-request
                    "Description\nend."
                    "GET"
                    "users"
                    '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                      ("header" . "value"))
                    "{\"login\": \"admin\"}")
                   (concat
                    "# Description\n"
                    "# end.\n"
                    "get users\n"
                    "Authorization: Basic dXNlcjE6UGFzc3dvcmQh\n"
                    "header: value\n"
                    "\n{\"login\": \"admin\"}\n")))))

(ert-deftest impostman-output-verb-footer ()
  "Test the output of verb footer."
  (should (equal (impostman-output-verb-footer "test")
                 (concat
                  "\n"
                  "* End of test\n"
                  "\n"
                  "# Local Variables:\n"
                  "# eval: (verb-mode)\n"
                  "# End:\n"))))

(ert-deftest impostman-output-restclient-header ()
  "Test the output of restclient header."
  (should (equal (impostman-output-restclient-header
                  "test" "Description\nLine 2")
                 (concat
                  "# -*- restclient -*-\n"
                  "#\n"
                  "# test\n"
                  "# Description\n"
                  "# Line 2\n"
                  "#\n"))))

(ert-deftest impostman-output-restclient-item ()
  "Test the output of restclient item."
  (should (equal (impostman-output-restclient-item 0 "test" "")
                 (concat
                  "\n"
                  "# test\n")))
  (should (equal (impostman-output-restclient-item 1 "test" "")
                 (concat
                  "\n"
                  "# test\n")))
  (should (equal (impostman-output-restclient-item 2 "test" "")
                 (concat
                  "\n"
                  "## test\n")))
  (should (equal (impostman-output-restclient-item 3 "test" "")
                 "### test\n"))
  (should (equal (impostman-output-restclient-item 0 "test" "Description\nend.")
                 (concat
                  "\n"
                  "# test\n"
                  "# Description\n"
                  "# end.\n")))
  (should (equal (impostman-output-restclient-item 1 "test" "Description\nend.")
                 (concat
                  "\n"
                  "# test\n"
                  "# Description\n"
                  "# end.\n")))
  (should (equal (impostman-output-restclient-item 2 "test" "Description\nend.")
                 (concat
                  "\n"
                  "## test\n"
                  "# Description\n"
                  "# end.\n")))
  (should (equal (impostman-output-restclient-item 3 "test" "Description\nend.")
                 (concat
                  "### test\n"
                  "# Description\n"
                  "# end.\n"))))

(ert-deftest impostman-output-restclient-request ()
  "Test the output of restclient request."
  (should (equal (impostman-output-restclient-request
                  ""
                  "GET"
                  "users"
                  nil
                  "")
                 "GET users\n"))
  (should (equal (impostman-output-restclient-request
                  ""
                  "GET"
                  "users"
                  nil
                  "{\"login\": \"admin\"}")
                 (concat
                  "GET users\n"
                  "{\"login\": \"admin\"}\n")))
  (should (equal (impostman-output-restclient-request
                  ""
                  "GET"
                  "users"
                  '(("Authorization" . "token"))
                  "{\"login\": \"admin\"}")
                 (concat
                  "GET users\n"
                  "Authorization: token\n"
                  "{\"login\": \"admin\"}\n")))
  (should (equal (impostman-output-restclient-request
                  "Description\nend."
                  "GET"
                  "users"
                  '(("Authorization" . "token") ("header" . "value"))
                  "{\"login\": \"admin\"}")
                 (concat
                  "# Description\n"
                  "# end.\n"
                  "GET users\n"
                  "Authorization: token\n"
                  "header: value\n"
                  "{\"login\": \"admin\"}\n")))
    (should (equal (impostman-output-restclient-request
                  "Description\nend."
                  "GET"
                  "users"
                  '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                    ("header" . "value"))
                  "{\"login\": \"admin\"}")
                 (concat
                  "# Description\n"
                  "# end.\n"
                  ":auth := (format \"Basic %s\" (base64-encode-string "
                  "(encode-coding-string \"user1:Password!\" 'utf-8) t))\n"
                  "GET users\n"
                  "Authorization: :auth\n"
                  "header: value\n"
                  "{\"login\": \"admin\"}\n")))
    (let ((impostman-auth-basic-as-elisp-code))
      (should (equal (impostman-output-restclient-request
                      "Description\nend."
                      "GET"
                      "users"
                      '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                        ("header" . "value"))
                      "{\"login\": \"admin\"}")
                     (concat
                      "# Description\n"
                      "# end.\n"
                      "GET users\n"
                      "Authorization: Basic dXNlcjE6UGFzc3dvcmQh\n"
                      "header: value\n"
                      "{\"login\": \"admin\"}\n")))))

(ert-deftest impostman-output-restclient-footer ()
  "Test the output of restclient footer."
  (should (equal (impostman-output-restclient-footer "test")
                 (concat
                  "\n"
                  "# End of test\n"))))

(ert-deftest impostman--build-auth-headers ()
  "Test the build of auth headers."
  (let ((auth (make-hash-table :test 'equal))
        (username #s(hash-table test equal data ("key" "username"
                                                 "value" "user1")))
        (password #s(hash-table test equal data ("key" "password"
                                                 "value" "Password!")))
        (apikey-key #s(hash-table test equal data ("key" "key"
                                                   "value" "apikey")))
        (apikey-value #s(hash-table test equal data ("key" "value"
                                                     "value" "1a2b3c4d")))
        (apikey-in #s(hash-table test equal data ("key" "in"
                                                  "value" "query"))))
    ;; no auth
    (should (equal (impostman--build-auth-headers nil)
                   nil))
    (should (equal (impostman--build-auth-headers
                    #s(hash-table test equal))
                   nil))
    (should (equal (impostman--build-auth-headers
                    #s(hash-table test equal data ("type" "unknown")))
                   nil))
    (should (equal (impostman--build-auth-headers
                    #s(hash-table test equal data ("type" "basic")))
                   nil))
    (should (equal (impostman--build-auth-headers
                    #s(hash-table test equal data ("type" "basic"
                                                   "basic" [])))
                   nil))
    (should (equal (impostman--build-auth-headers auth)
                   nil))
    ;; test basic
    (puthash "type" "basic" auth)
    (puthash "basic" [] auth)
    (puthash "basic" (vector username) auth)
    (should (equal (impostman--build-auth-headers auth)
                   `(("Authorization" .
                      ,(concat "Basic "
                               (base64-encode-string "user1:"))))))
    (puthash "basic" (vector password) auth)
    (should (equal (impostman--build-auth-headers auth)
                   `(("Authorization" .
                      ,(concat "Basic "
                               (base64-encode-string ":Password!"))))))
    (puthash "basic" (vector username password) auth)
    (should (equal (impostman--build-auth-headers auth)
                   `(("Authorization" .
                      ,(concat "Basic "
                               (base64-encode-string "user1:Password!"))))))
    ;; test apikey
    (clrhash auth)
    (puthash "type" "apikey" auth)
    (puthash "apikey" (vector apikey-key apikey-value) auth)
    (should (equal (impostman--build-auth-headers auth)
                   '(("apikey" . "1a2b3c4d"))))
    ;; test apikey in query-string (no header)
    (puthash "apikey" (vector apikey-key apikey-value apikey-in) auth)
    (should (equal (impostman--build-auth-headers auth)
                   nil))))

(ert-deftest impostman--build-headers ()
  "Test the build of headers."
  (let ((header1 #s(hash-table test equal data ("key" "header1"
                                                "value" "value1")))
        (header2 #s(hash-table test equal data ("key" "X-header2"
                                                "value" "the value 2"))))
    ;; no header
    (should (equal (impostman--build-headers nil)
                   nil))
    (should (equal (impostman--build-headers [])
                   nil))
    ;; test headers
    (should (equal (impostman--build-headers (vector header1 header2))
                   '(("header1" . "value1")
                     ("X-header2" . "the value 2"))))
    (should (equal (impostman--build-headers
                    (vector header1 header2 header1 header2))
                   '(("header1" . "value1")
                     ("X-header2" . "the value 2")
                     ("header1" . "value1")
                     ("X-header2" . "the value 2"))))))

(ert-deftest impostman--build-auth-query-string ()
  "Test build of query-string parameter for authentication."
  (let ((auth (make-hash-table :test 'equal))
        (apikey-key #s(hash-table test equal data ("key" "key"
                                                   "value" "apikey")))
        (apikey-value #s(hash-table test equal data ("key" "value"
                                                     "value" "1a2b3c4d")))
        (apikey-in #s(hash-table test equal data ("key" "in"
                                                  "value" "query"))))
    ;; test apikey as header (no query-string)
    (puthash "type" "apikey" auth)
    (puthash "apikey" (vector apikey-key apikey-value) auth)
    (should (equal (impostman--build-auth-query-string auth)
                   nil))
    ;; test apikey as query-string
    (puthash "apikey" (vector apikey-key apikey-value apikey-in) auth)
    (should (equal (impostman--build-auth-query-string auth)
                   '(("apikey" . "1a2b3c4d"))))))

(ert-deftest impostman--parse-item ()
  "Test parsing of an item."
  (let ((item1 (make-hash-table :test 'equal))
        (request1 (make-hash-table :test 'equal))
        (url (make-hash-table :test 'equal))
        (body (make-hash-table :test 'equal))
        (auth (make-hash-table :test 'equal))
        (username #s(hash-table test equal data ("key" "username"
                                                 "value" "user1")))
        (password #s(hash-table test equal data ("key" "password"
                                                 "value" "Password!")))
        (header1 #s(hash-table test equal data ("key" "header1"
                                                "value" "value1")))
        (header2 #s(hash-table test equal data ("key" "X-header2"
                                                "value" "the value 2"))))
    (puthash "name" "item1" item1)
    ;; item without request
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"))))
    ;; add description in item
    (puthash "description" "The description\nline 2." item1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"
                      "# The description\n"
                      "# line 2.\n"))))
    ;; item with a request
    (puthash "method" "POST" request1)
    (puthash "raw" "https://example.com" url)
    (puthash "url" url request1)
    (puthash "request" request1 item1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"
                      "# The description\n"
                      "# line 2.\n"
                      "post https://example.com\n"))))
    ;; add description in request
    (puthash "description" "Some info on request" request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"
                      "# The description\n"
                      "# line 2.\n"
                      "# Some info on request\n"
                      "post https://example.com\n"))))
    ;; add auth in request
    (puthash "type" "basic" auth)
    (puthash "basic" [] auth)
    (puthash "basic" (vector username password) auth)
    (puthash "auth" auth request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"
                      "# The description\n"
                      "# line 2.\n"
                      "# Some info on request\n"
                      "post https://example.com\n"
                      "Authorization: Basic {{(base64-encode-string "
                      "(encode-coding-string \"user1:Password!\" "
                      "'utf-8) t)}}\n"))))
    ;; add 2 headers in request
    (puthash "header" (vector header1 header2) request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"
                      "# The description\n"
                      "# line 2.\n"
                      "# Some info on request\n"
                      "post https://example.com\n"
                      "Authorization: Basic {{(base64-encode-string "
                      "(encode-coding-string \"user1:Password!\" "
                      "'utf-8) t)}}\n"
                      "header1: value1\n"
                      "X-header2: the value 2\n"))))
    ;; add body in request
    (puthash "raw" "{\"key\": \"data\"}" body)
    (puthash "body" body request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (concat
                      "\n"
                      "** item1\n"
                      "# The description\n"
                      "# line 2.\n"
                      "# Some info on request\n"
                      "post https://example.com\n"
                      "Authorization: Basic {{(base64-encode-string "
                      "(encode-coding-string \"user1:Password!\" "
                      "'utf-8) t)}}\n"
                      "header1: value1\n"
                      "X-header2: the value 2\n"
                      "\n"
                      "{\"key\": \"data\"}\n"))))))

(ert-deftest impostman--parse-json ()
  "Test parsing of a JSON collection."
  (let ((collection (make-hash-table :test 'equal))
        (info (make-hash-table :test 'equal)))
    ;; empty collection
    (save-excursion
      (impostman--parse-json collection impostman-output-verb-alist)
      (should (string-prefix-p "unknown.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (concat
                        "* unknown  :verb:\n"
                        "\n"
                        "* End of unknown\n"
                        "\n"
                        "# Local Variables:\n"
                        "# eval: (verb-mode)\n"
                        "# End:\n"))))
      (kill-this-buffer))
    ;; add a name in collection
    (puthash "name" "my_collection" info)
    (puthash "info" info collection)
    (save-excursion
      (impostman--parse-json collection impostman-output-verb-alist)
      (should (string-prefix-p "my_collection.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (concat
                        "* my_collection  :verb:\n"
                        "\n"
                        "* End of my_collection\n"
                        "\n"
                        "# Local Variables:\n"
                        "# eval: (verb-mode)\n"
                        "# End:\n"))))
      (kill-this-buffer))
    ;; add a description in collection
    (puthash "description" "Description\nLine 2" info)
    (save-excursion
      (impostman--parse-json collection impostman-output-verb-alist)
      (should (string-prefix-p "my_collection.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (concat
                        "* my_collection  :verb:\n"
                        "# Description\n"
                        "# Line 2\n"
                        "\n"
                        "* End of my_collection\n"
                        "\n"
                        "# Local Variables:\n"
                        "# eval: (verb-mode)\n"
                        "# End:\n"))))
      (kill-this-buffer))))

(ert-deftest impostman-import-file ()
  "Test import of a file with a Postman collection."
  (let ((verb-output)
        (restclient-output))
    (with-temp-buffer
      (insert-file-contents "tests/verb.org")
      (setq verb-output (buffer-string)))
    (with-temp-buffer
      (insert-file-contents "tests/restclient.org")
      (setq restclient-output (buffer-string)))
    (save-excursion
      (impostman-import-file "tests/collection.json" "verb")
      (should (string-prefix-p "test.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result verb-output)))
      (kill-this-buffer))
    (save-excursion
      (impostman-import-file "tests/collection.json" "restclient")
      (should (string-prefix-p "test.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result restclient-output)))
      (kill-this-buffer))))

(ert-deftest impostman-import-string ()
  "Test import of a string with a Postman collection."
  (let ((collection)
        (verb-output)
        (restclient-output))
    (with-temp-buffer
      (insert-file-contents "tests/collection.json")
      (setq collection (buffer-string)))
    (with-temp-buffer
      (insert-file-contents "tests/verb.org")
      (setq verb-output (buffer-string)))
    (with-temp-buffer
      (insert-file-contents "tests/restclient.org")
      (setq restclient-output (buffer-string)))
    (save-excursion
      (impostman-import-string collection "verb")
      (should (string-prefix-p "test.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result verb-output)))
      (kill-this-buffer))
    (save-excursion
      (impostman-import-string collection "restclient")
      (should (string-prefix-p "test.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result restclient-output)))
      (kill-this-buffer))))

;;; impostman-tests.el ends here
