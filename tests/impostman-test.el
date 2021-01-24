;;; impostman-test.el --- Test suite for impostman  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Sébastien Helleu <flashcode@flashtux.org>

;; Author: Sébastien Helleu <flashcode@flashtux.org>
;; Maintainer: Sébastien Helleu <flashcode@flashtux.org>
;; Created: 2020-12-24
;; Keywords: tools
;; URL: https://github.com/flashcode/impostman
;; Package-Version: 0.2.0-snapshot
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

;; Test suite for impostman.

;;; Code:

(require 'ert)
(require 'impostman)

(defun impostman-test--get-file-contents (filename)
  "Get contents of filename."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(ert-deftest impostman-test-format-comment ()
  "Test the format of comment."
  ;; without prefix: default is "# "
  (should (equal (impostman-format-comment nil)
                 ""))
  (should (equal (impostman-format-comment "")
                 ""))
  (should (equal (impostman-format-comment "Test")
                 (string-join '("# Test" "") "\n")))
  (should (equal (impostman-format-comment
                  (string-join '("Test" "Line 2") "\n"))
                 (string-join '("# Test" "# Line 2" "") "\n")))
  (should (equal (impostman-format-comment
                  (string-join '("Test" "Line 2" "Line 3") "\n"))
                 (string-join '("# Test" "# Line 2" "# Line 3" "") "\n")))

  ;; with a prefix
  (should (equal (impostman-format-comment "" ";; ")
                 ""))
  (should (equal (impostman-format-comment "Test" ";; ")
                 (string-join '(";; Test" "") "\n")))
  (should (equal (impostman-format-comment
                  (string-join '("Test" "Line 2") "\n") ";; ")
                 (string-join '(";; Test" ";; Line 2" "") "\n")))
  (should (equal (impostman-format-comment
                  (string-join '("Test" "Line 2" "Line 3") "\n") ";; ")
                 (string-join '(";; Test" ";; Line 2" ";; Line 3" "") "\n"))))

(ert-deftest impostman-test-get-auth-basic-plain ()
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

(ert-deftest impostman-test-output-verb-replace-vars ()
  "Test the format of variables with verb syntax."
  (let ((impostman-use-variables t))
    (should (equal (impostman-output-verb-replace-vars
                    nil nil)
                   ""))
    (should (equal (impostman-output-verb-replace-vars
                    "" nil)
                   ""))
    (should (equal (impostman-output-verb-replace-vars
                    "Test" nil)
                   "Test"))
    (should (equal (impostman-output-verb-replace-vars
                    "Test {{var1}} and {{var2}}" nil)
                   "Test {{(verb-var var1)}} and {{(verb-var var2)}}"))
    (should (equal (impostman-output-verb-replace-vars
                    "Test {{var1}} and {{var2}}"
                    '(("var1" . "value1") ("var2" . "value2")))
                   "Test {{(verb-var var1)}} and {{(verb-var var2)}}")))
  (let (impostman-use-variables)
    (should (equal (impostman-output-verb-replace-vars
                    "Test {{var1}}, {{var2}} and {{var3}}"
                    '(("var1" . "value1") ("var2" . "value2")))
                 "Test value1, value2 and var3"))))

(ert-deftest impostman-test-output-verb-header ()
  "Test the output of verb header."
  (should (equal (impostman-output-verb-header
                  "test" (string-join '("Description" "Line 2") "\n") nil)
                 (string-join
                  '("* test  :verb:"
                    "# Description"
                    "# Line 2"
                    "")
                  "\n"))))

(ert-deftest impostman-test-output-verb-item ()
  "Test the output of verb item."
  (should (equal (impostman-output-verb-item 0 "test" "" nil)
                 (string-join '("" "* test" "") "\n")))
  (should (equal (impostman-output-verb-item 1 "test" "" nil)
                 (string-join '("" "* test" "") "\n")))
  (should (equal (impostman-output-verb-item 2 "test" "" nil)
                 (string-join '("" "** test" "") "\n")))
  (should (equal (impostman-output-verb-item 3 "test" "" nil)
                 (string-join '("*** test" "") "\n")))
  (should (equal (impostman-output-verb-item
                  0
                  "test"
                  (string-join '("line 2" "end.") "\n")
                  nil)
                 (string-join '("" "* test" "# line 2" "# end." "") "\n")))
  (should (equal (impostman-output-verb-item
                  1
                  "test"
                  (string-join '("line 2" "end.") "\n")
                  nil)
                 (string-join '("" "* test" "# line 2" "# end." "") "\n")))
  (should (equal (impostman-output-verb-item
                  2
                  "test"
                  (string-join '("line 2" "end.") "\n")
                  nil)
                 (string-join '("" "** test" "# line 2" "# end." "") "\n")))
  (should (equal (impostman-output-verb-item
                  3
                  "test"
                  (string-join '("line 2" "end.") "\n")
                  nil)
                 (string-join '("*** test" "# line 2" "# end." "") "\n"))))

(ert-deftest impostman-test-output-verb-request ()
  "Test the output of verb request."
  (let ((impostman-auth-basic-as-elisp-code t))
    (should (equal (impostman-output-verb-request
                    ""
                    "GET"
                    "users"
                    nil
                    ""
                    nil)
                   (string-join '("get users" "") "\n")))
    (should (equal (impostman-output-verb-request
                    ""
                    "POST"
                    "users"
                    nil
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("post users"
                      ""
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))
    (should (equal (impostman-output-verb-request
                    ""
                    "POST"
                    "users"
                    '(("Authorization" . "token"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("post users"
                      "Authorization: token"
                      ""
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))
    (should (equal (impostman-output-verb-request
                    (string-join '("Description" "end.") "\n")
                    "POST"
                    "users"
                    '(("Authorization" . "token") ("header" . "value"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("# Description"
                      "# end."
                      "post users"
                      "Authorization: token"
                      "header: value"
                      ""
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))
    (should (equal (impostman-output-verb-request
                    (string-join '("Description" "end.") "\n")
                    "POST"
                    "users"
                    '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                      ("header" . "value"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("# Description"
                      "# end."
                      "post users"
                      "Authorization: Basic {{(base64-encode-string (encode-coding-string \"user1:Password!\" 'utf-8) t)}}"
                      "header: value"
                      ""
                      "{\"login\": \"admin\"}"
                      "")
                    "\n"))))
  (let (impostman-auth-basic-as-elisp-code)
    (should (equal (impostman-output-verb-request
                    (string-join '("Description" "end.") "\n")
                    "POST"
                    "users"
                    '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                      ("header" . "value"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("# Description"
                      "# end."
                      "post users"
                      "Authorization: Basic dXNlcjE6UGFzc3dvcmQh"
                      "header: value"
                      ""
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))))

(ert-deftest impostman-test-output-verb-footer ()
  "Test the output of verb footer."
  (let ((impostman-use-variables t))
    (should (equal (impostman-output-verb-footer "test" nil)
                   (string-join
                    '(""
                      "* End of test"
                      ""
                      "# Local Variables:"
                      "# eval: (verb-mode)"
                      "# End:"
                      "")
                    "\n")))
    (should (equal (impostman-output-verb-footer
                    "test"
                    '(("var1" . "value1") ("var2" . "value2")))
                   (string-join
                    '(""
                      "* End of test"
                      ""
                      "# Local Variables:"
                      "# eval: (verb-mode)"
                      "# eval: (verb-set-var \"var1\" \"value1\")"
                      "# eval: (verb-set-var \"var2\" \"value2\")"
                      "# End:"
                      "")
                    "\n"))))
  (let (impostman-use-variables)
    (should (equal (impostman-output-verb-footer
                    "test"
                    '(("var1" . "value1") ("var2" . "value2")))
                   (string-join
                    '(""
                      "* End of test"
                      ""
                      "# Local Variables:"
                      "# eval: (verb-mode)"
                      "# End:"
                      "")
                    "\n")))))

(ert-deftest impostman-test-output-restclient-replace-vars ()
  "Test the format of variables with restclient syntax."
  (let ((impostman-use-variables t))
    (should (equal (impostman-output-restclient-replace-vars
                    nil nil)
                   ""))
    (should (equal (impostman-output-restclient-replace-vars
                    "" nil)
                   ""))
    (should (equal (impostman-output-restclient-replace-vars
                    "Test" nil)
                   "Test"))
    (should (equal (impostman-output-restclient-replace-vars
                    "Test {{var1}} and {{var2}}" nil)
                   "Test :var1 and :var2"))
    (should (equal (impostman-output-restclient-replace-vars
                    "Test {{var1}} and {{var2}}"
                    '(("var1" . "value1") ("var2" . "value2")))
                   "Test :var1 and :var2")))
  (let (impostman-use-variables)
    (should (equal (impostman-output-restclient-replace-vars
                    "Test {{var1}}, {{var2}} and {{var3}}"
                    '(("var1" . "value1") ("var2" . "value2")))
                 "Test value1, value2 and var3"))))

(ert-deftest impostman-test-output-restclient-header ()
  "Test the output of restclient header."
  (let ((impostman-use-variables t))
    (should (equal (impostman-output-restclient-header
                    "test" (string-join '("Description" "Line 2") "\n") nil)
                   (string-join
                    '("# -*- restclient -*-"
                      "#"
                      "# test"
                      "# Description"
                      "# Line 2"
                      "#"
                      "")
                    "\n")))
    (should (equal (impostman-output-restclient-header
                    "test"
                    (string-join '("Description" "Line 2") "\n")
                    '(("var1" . "value1") ("var2" . "value2")))
                   (string-join
                    '("# -*- restclient -*-"
                      "#"
                      "# test"
                      "# Description"
                      "# Line 2"
                      "#"
                      ""
                      ":var1 = value1"
                      ":var2 = value2"
                      "")
                    "\n"))))
  (let (impostman-use-variables)
      (should (equal (impostman-output-restclient-header
                      "test"
                      (string-join '("Description" "Line 2") "\n")
                      '(("var1" . "value1") ("var2" . "value2")))
                     (string-join
                      '("# -*- restclient -*-"
                        "#"
                        "# test"
                        "# Description"
                        "# Line 2"
                        "#"
                        "")
                      "\n")))))

(ert-deftest impostman-test-output-restclient-item ()
  "Test the output of restclient item."
  (should (equal (impostman-output-restclient-item 0 "test" "" nil)
                 (string-join '("" "# test" "") "\n")))
  (should (equal (impostman-output-restclient-item 1 "test" "" nil)
                 (string-join '("" "# test" "") "\n")))
  (should (equal (impostman-output-restclient-item 2 "test" "" nil)
                 (string-join '("" "## test" "") "\n")))
  (should (equal (impostman-output-restclient-item 3 "test" "" nil)
                 (string-join '("### test" "") "\n")))
  (should (equal (impostman-output-restclient-item
                  0 "test" (string-join '("Description" "end.") "\n") nil)
                 (string-join
                  '(""
                    "# test"
                    "# Description"
                    "# end."
                    "")
                  "\n")))
  (should (equal (impostman-output-restclient-item
                  1 "test" (string-join '("Description" "end.") "\n") nil)
                 (string-join
                  '(""
                    "# test"
                    "# Description"
                    "# end."
                    "")
                  "\n")))
  (should (equal (impostman-output-restclient-item
                  2 "test" (string-join '("Description" "end.") "\n") nil)
                 (string-join
                  '(""
                    "## test"
                    "# Description"
                    "# end."
                    "")
                  "\n")))
  (should (equal (impostman-output-restclient-item
                  3 "test" (string-join '("Description" "end.") "\n") nil)
                 (string-join
                  '("### test"
                    "# Description"
                    "# end."
                    "")
                  "\n"))))

(ert-deftest impostman-test-output-restclient-request ()
  "Test the output of restclient request."
  (let ((impostman-auth-basic-as-elisp-code t))
    (should (equal (impostman-output-restclient-request
                    ""
                    "GET"
                    "users"
                    nil
                    ""
                    nil)
                   (string-join
                    '("GET users"
                      "")
                    "\n")))
    (should (equal (impostman-output-restclient-request
                    ""
                    "GET"
                    "users"
                    nil
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("GET users"
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))
    (should (equal (impostman-output-restclient-request
                    ""
                    "GET"
                    "users"
                    '(("Authorization" . "token"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("GET users"
                      "Authorization: token"
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))
    (should (equal (impostman-output-restclient-request
                    (string-join '("Description" "end.") "\n")
                    "GET"
                    "users"
                    '(("Authorization" . "token") ("header" . "value"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("# Description"
                      "# end."
                      "GET users"
                      "Authorization: token"
                      "header: value"
                      "{\"login\": \"admin\"}"
                      "")
                    "\n")))
    (should (equal (impostman-output-restclient-request
                    (string-join '("Description" "end.") "\n")
                    "GET"
                    "users"
                    '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                      ("header" . "value"))
                    "{\"login\": \"admin\"}"
                    nil)
                   (string-join
                    '("# Description"
                      "# end."
                      ":auth := (format \"Basic %s\" (base64-encode-string (encode-coding-string \"user1:Password!\" 'utf-8) t))"
                      "GET users"
                      "Authorization: :auth"
                      "header: value"
                      "{\"login\": \"admin\"}"
                      "")
                    "\n"))))
    (let (impostman-auth-basic-as-elisp-code)
      (should (equal (impostman-output-restclient-request
                      (string-join '("Description" "end.") "\n")
                      "GET"
                      "users"
                      '(("Authorization" . "Basic dXNlcjE6UGFzc3dvcmQh")
                        ("header" . "value"))
                      "{\"login\": \"admin\"}"
                      nil)
                     (string-join
                      '("# Description"
                        "# end."
                        "GET users"
                        "Authorization: Basic dXNlcjE6UGFzc3dvcmQh"
                        "header: value"
                        "{\"login\": \"admin\"}"
                        "")
                      "\n")))))

(ert-deftest impostman-test-output-restclient-footer ()
  "Test the output of restclient footer."
  (should (equal (impostman-output-restclient-footer "test" nil)
                 (string-join
                  '(""
                    "# End of test"
                    "")
                  "\n"))))

(ert-deftest impostman-test-build-auth-headers ()
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

(ert-deftest impostman-test-build-headers ()
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

(ert-deftest impostman-test-build-auth-query-string ()
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

(ert-deftest impostman-test-add-query-string-items-to-url ()
  "Test add of query-string items to an URL."
  (should (equal (impostman--add-query-string-items-to-url
                  "https://example.com" nil)
                 "https://example.com"))
  (should (equal (impostman--add-query-string-items-to-url
                  "https://example.com" '(("apikey" . "1a2b3c4d")))
                 "https://example.com?apikey=1a2b3c4d"))
  (should (equal (impostman--add-query-string-items-to-url
                  "https://example.com?a=1" '(("apikey" . "1a2b3c4d")))
                 "https://example.com?a=1&apikey=1a2b3c4d")))

(ert-deftest impostman-test-build-variables ()
  "Test build of variables."
  (let ((var1 #s(hash-table test equal data ("key" "var1"
                                             "value" "value1")))
        (var2 #s(hash-table test equal data ("key" "var2"
                                             "value" "value2"
                                             "enabled" t)))
        (var3 #s(hash-table test equal data ("key" "var3"
                                             "value" "value2"
                                             "enabled" :false))))
    (should (equal (impostman--build-variables nil)
                   nil))
    (should (equal (impostman--build-variables [])
                   nil))
    (should (equal (impostman--build-variables (vector var1))
                   '(("var1" . "value1"))))
    (should (equal (impostman--build-variables (vector var1 var2))
                   '(("var1" . "value1") ("var2" . "value2"))))
    (should (equal (impostman--build-variables (vector var2 var1))
                   '(("var2" . "value2") ("var1" . "value1"))))
    (should (equal (impostman--build-variables (vector var2 var1 var3))
                   '(("var2" . "value2") ("var1" . "value1"))))))

(ert-deftest impostman-test-parse-item ()
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
      (impostman--parse-item (vector item1) 2 nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "")
                      "\n"))))
    ;; add description in item
    (puthash "description"
             (string-join '("The description" "line 2.") "\n") item1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "# The description"
                        "# line 2."
                        "")
                      "\n"))))
    ;; item with a request
    (puthash "method" "POST" request1)
    (puthash "raw" "https://example.com" url)
    (puthash "url" url request1)
    (puthash "request" request1 item1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2  nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "# The description"
                        "# line 2."
                        "post https://example.com"
                        "")
                      "\n"))))
    ;; add description in request
    (puthash "description" "Some info on request" request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "# The description"
                        "# line 2."
                        "# Some info on request"
                        "post https://example.com"
                        "")
                      "\n"))))
    ;; add auth in request
    (puthash "type" "basic" auth)
    (puthash "basic" [] auth)
    (puthash "basic" (vector username password) auth)
    (puthash "auth" auth request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "# The description"
                        "# line 2."
                        "# Some info on request"
                        "post https://example.com"
                        "Authorization: Basic {{(base64-encode-string (encode-coding-string \"user1:Password!\" 'utf-8) t)}}"
                        "")
                      "\n"))))
    ;; add 2 headers in request
    (puthash "header" (vector header1 header2) request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "# The description"
                        "# line 2."
                        "# Some info on request"
                        "post https://example.com"
                        "Authorization: Basic {{(base64-encode-string (encode-coding-string \"user1:Password!\" 'utf-8) t)}}"
                        "header1: value1"
                        "X-header2: the value 2"
                        "")
                      "\n"))))
    ;; add body in request
    (puthash "raw" "{\"key\": \"data\"}" body)
    (puthash "body" body request1)
    (with-temp-buffer
      (impostman--parse-item (vector item1) 2 nil impostman-output-verb-alist)
      (should (equal (buffer-string)
                     (string-join
                      '(""
                        "** item1"
                        "# The description"
                        "# line 2."
                        "# Some info on request"
                        "post https://example.com"
                        "Authorization: Basic {{(base64-encode-string (encode-coding-string \"user1:Password!\" 'utf-8) t)}}"
                        "header1: value1"
                        "X-header2: the value 2"
                        ""
                        "{\"key\": \"data\"}"
                        "")
                      "\n"))))))

(ert-deftest impostman-test-parse-json ()
  "Test parsing of a JSON collection."
  (let ((collection (make-hash-table :test 'equal))
        (info (make-hash-table :test 'equal))
        (environment (make-hash-table :test 'equal))
        (var1 #s(hash-table test equal data ("key" "var1"
                                             "value" "value1"
                                             "enabled" t)))
        (var2 #s(hash-table test equal data ("key" "var2"
                                             "value" "value2"
                                             "enabled" t))))
    ;; empty collection / environment
    (save-excursion
      (impostman--parse-json collection environment impostman-output-verb-alist)
      (should (string-prefix-p "unknown.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (string-join
                        '("* unknown  :verb:"
                          ""
                          "* End of unknown"
                          ""
                          "# Local Variables:"
                          "# eval: (verb-mode)"
                          "# End:"
                          "")
                        "\n"))))
      (kill-this-buffer))
    ;; add a name in collection
    (puthash "name" "my_collection" info)
    (puthash "info" info collection)
    (save-excursion
      (impostman--parse-json collection environment impostman-output-verb-alist)
      (should (string-prefix-p "my_collection.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (string-join
                        '("* my_collection  :verb:"
                          ""
                          "* End of my_collection"
                          ""
                          "# Local Variables:"
                          "# eval: (verb-mode)"
                          "# End:"
                          "")
                        "\n"))))
      (kill-this-buffer))
    ;; add a description in collection
    (puthash "description" (string-join '("Description" "Line 2") "\n") info)
    (save-excursion
      (impostman--parse-json collection environment impostman-output-verb-alist)
      (should (string-prefix-p "my_collection.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (string-join
                        '("* my_collection  :verb:"
                          "# Description"
                          "# Line 2"
                          ""
                          "* End of my_collection"
                          ""
                          "# Local Variables:"
                          "# eval: (verb-mode)"
                          "# End:"
                          "")
                        "\n"))))
      (kill-this-buffer))
    ;; add an environment
    (puthash "values" (vector var1 var2) environment)
    (save-excursion
      (impostman--parse-json collection environment impostman-output-verb-alist)
      (should (string-prefix-p "my_collection.org" (buffer-name)))
      (let ((result (buffer-string)))
        (should (equal result
                       (string-join
                        '("* my_collection  :verb:"
                          "# Description"
                          "# Line 2"
                          ""
                          "* End of my_collection"
                          ""
                          "# Local Variables:"
                          "# eval: (verb-mode)"
                          "# eval: (verb-set-var \"var1\" \"value1\")"
                          "# eval: (verb-set-var \"var2\" \"value2\")"
                          "# End:"
                          "")
                        "\n"))))
      (kill-this-buffer))))

(defun impostman-test-output-custom-init (variables)
  "Initialize custom output."
  (ignore variables))

(defun impostman-test-output-custom-replace-vars (string variables)
  "Format variables for custom output."
  (if impostman-use-variables
      (replace-regexp-in-string
       "{{\\([^}]+\\)}}" "$(\\1)" (or string ""))
    (replace-regexp-in-string
     "{{[^}]+}}"
     (lambda (s)
       (let* ((name (substring s 2 -2))
              (var (assoc name variables)))
         (if var (cdr var) name)))
     (or string ""))))

(defun impostman-test-output-custom-header (name description variables)
  "Format header for custom output."
  (let (list-vars)
    (when impostman-use-variables
      (dolist (var variables)
        (push (format "VAR(%s) = %s" (car var) (cdr var)) list-vars)))
    (concat
     "* " name "  :test:" "\n"
     (impostman-format-comment description)
     (when list-vars
       (concat "\n" (string-join (nreverse list-vars) "\n") "\n")))))

(defun impostman-test-output-custom-item (level name description variables)
  "Format item for custom output."
  (ignore variables)
  (concat
   (if (<= level 2) "\n" "")
   (make-string (max level 1) ?*) " " name "\n"
   (impostman-format-comment description)))

(defun impostman-test-output-custom-request (description method url headers body variables)
  "Format request for custom output."
  (ignore variables)
  (let (list-headers)
    (dolist (header headers)
      (push (format "%s: %s" (car header) (cdr header)) list-headers))
    (concat
     (impostman-format-comment description)
     method " " url "\n"
     (when list-headers
       (concat (string-join (nreverse list-headers) "\n") "\n"))
     (if (string-empty-p body) "" (concat body "\n")))))

(defun impostman-test-output-custom-footer (name variables)
  "Format footer for custom output."
  (ignore variables)
  (concat "\n" "* End of " name "\n"))

(defun impostman-test-output-custom-end (variables)
  "End of custom output."
  (ignore variables))

(ert-deftest impostman-test-import-file ()
  "Test import of a file with a Postman collection."
  (defvar impostman-test-output-custom-alist
    '((init . impostman-test-output-custom-init)
      (replace-vars . impostman-test-output-custom-replace-vars)
      (header . impostman-test-output-custom-header)
      (item . impostman-test-output-custom-item)
      (request . impostman-test-output-custom-request)
      (footer . impostman-test-output-custom-footer)
      (end . impostman-test-output-custom-end)))
  (let* ((verb-output
          (impostman-test--get-file-contents "verb.org"))
         (verb-no-vars-output
          (impostman-test--get-file-contents "verb_no_vars.org"))
         (restclient-output
          (impostman-test--get-file-contents "restclient.org"))
         (restclient-no-vars-output
          (impostman-test--get-file-contents "restclient_no_vars.org"))
         (custom-output
          (impostman-test--get-file-contents "custom.org"))
         (custom-no-vars-output
          (impostman-test--get-file-contents "custom_no_vars.org"))
         (impostman-outputs-alist
          '(("verb" . impostman-output-verb-alist)
            ("restclient" . impostman-output-restclient-alist)
            ("custom" . impostman-test-output-custom-alist))))
    (let ((impostman-use-variables t))
      (save-excursion
        (impostman-import-file "httpbin.postman_collection.json"
                               "httpbin.postman_environment.json"
                               "verb")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result verb-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-file "httpbin.postman_collection.json"
                               "httpbin.postman_environment.json"
                               "restclient")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result restclient-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-file "httpbin.postman_collection.json"
                               "httpbin.postman_environment.json"
                               "custom")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result custom-output)))
        (kill-this-buffer)))
    (let (impostman-use-variables)
      (save-excursion
        (impostman-import-file "httpbin.postman_collection.json"
                               "httpbin.postman_environment.json"
                               "verb")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result verb-no-vars-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-file "httpbin.postman_collection.json"
                               "httpbin.postman_environment.json"
                               "restclient")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result restclient-no-vars-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-file "httpbin.postman_collection.json"
                               "httpbin.postman_environment.json"
                               "custom")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result custom-no-vars-output)))
        (kill-this-buffer))))
  (makunbound 'impostman-output-test-alist))

(ert-deftest impostman-test-import-string ()
  "Test import of a string with a Postman collection."
  (defvar impostman-test-output-custom-alist
    '((init . impostman-test-output-custom-init)
      (replace-vars . impostman-test-output-custom-replace-vars)
      (header . impostman-test-output-custom-header)
      (item . impostman-test-output-custom-item)
      (request . impostman-test-output-custom-request)
      (footer . impostman-test-output-custom-footer)
      (end . impostman-test-output-custom-end)))
  (let* ((collection
          (impostman-test--get-file-contents "httpbin.postman_collection.json"))
         (environment
          (impostman-test--get-file-contents "httpbin.postman_environment.json"))
         (verb-output
          (impostman-test--get-file-contents "verb.org"))
         (verb-no-vars-output
          (impostman-test--get-file-contents "verb_no_vars.org"))
         (restclient-output
          (impostman-test--get-file-contents "restclient.org"))
         (restclient-no-vars-output
          (impostman-test--get-file-contents "restclient_no_vars.org"))
         (custom-output
          (impostman-test--get-file-contents "custom.org"))
         (custom-no-vars-output
          (impostman-test--get-file-contents "custom_no_vars.org"))
         (impostman-outputs-alist
          '(("verb" . impostman-output-verb-alist)
            ("restclient" . impostman-output-restclient-alist)
            ("custom" . impostman-test-output-custom-alist))))
    (let ((impostman-use-variables t))
      (save-excursion
        (impostman-import-string collection environment "verb")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result verb-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-string collection environment "restclient")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result restclient-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-string collection environment "custom")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result custom-output)))
        (kill-this-buffer)))
    (let (impostman-use-variables)
      (save-excursion
        (impostman-import-string collection environment "verb")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result verb-no-vars-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-string collection environment "restclient")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result restclient-no-vars-output)))
        (kill-this-buffer))
      (save-excursion
        (impostman-import-string collection environment "custom")
        (should (string-prefix-p "httpbin.org" (buffer-name)))
        (let ((result (buffer-string)))
          (should (equal result custom-no-vars-output)))
        (kill-this-buffer))))
  (makunbound 'impostman-output-test-alist))

(ert-deftest impostman-test-version ()
  "Test Impostman version."
  (should (equal (impostman-version) impostman-version)))

(provide 'impostman-test)

;;; impostman-test.el ends here
