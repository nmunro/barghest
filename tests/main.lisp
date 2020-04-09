(defpackage barghest/tests/main
  (:use :cl
        :barghest
        :rove))
(in-package :barghest/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :barghest)' in your Lisp.

(deftest test-decode-param-space
  (testing "Decoding param %20 should be \" \""
    (ok (string= (barghest:decode-param "%20") " "))))

(deftest test-decode-param-<
  (testing "Decoding param %3C should be \"<\""
    (ok (string= (barghest:decode-param "%3C") "<"))))

(deftest test-decode-param->
  (testing "Decoding param %3E should be \">\""
    (ok (string= (barghest:decode-param "%3E") ">"))))

(deftest test-decode-param-#
  (testing "Decoding param %23 should be \"#\""
    (ok (string= (barghest:decode-param "%23") "#"))))

(deftest test-decode-param-%
  (testing "Decoding param %25 should be \"#\""
    (ok (string= (barghest:decode-param "%25") "%"))))

(deftest test-decode-param-{
  (testing "Decoding param %7B should be \"{\""
    (ok (string= (barghest:decode-param "%7B") "{"))))

(deftest test-decode-param-}
  (testing "Decoding param %7B should be \"}\""
    (ok (string= (barghest:decode-param "%7D") "}"))))

(deftest test-decode-param-pipeline
  (testing "Decoding param %7C should be \"|\""
    (ok (string= (barghest:decode-param "%7C") "|"))))

(deftest test-decode-param-backslash
  (testing "Decoding param %5C should be \"\\\""
    (ok (string= (barghest:decode-param "%5C") "\\"))))

(deftest test-decode-param-^
  (testing "Decoding param %5E should be \"^\""
    (ok (string= (barghest:decode-param "%5E") "^"))))

(deftest test-decode-param-~
  (testing "Decoding param %7E should be \"~\""
    (ok (string= (barghest:decode-param "%7E") "~"))))

(deftest test-decode-param-[
  (testing "Decoding param %5B should be \"[\""
    (ok (string= (barghest:decode-param "%5B") "["))))

(deftest test-decode-param-]
  (testing "Decoding param %5D should be \"]\""
    (ok (string= (barghest:decode-param "%5D") "]"))))

(deftest test-decode-param-backtick
  (testing "Decoding param %60 should be \"`\""
    (ok (string= (barghest:decode-param "%60") "`"))))

(deftest test-decode-param-semicolon
  (testing "Decoding param %3B should be \";\""
    (ok (string= (barghest:decode-param "%3B") ";"))))

(deftest test-decode-param-/
  (testing "Decoding param %2F should be \"/\""
    (ok (string= (barghest:decode-param "%2F") "/"))))

(deftest test-decode-param-?
  (testing "Decoding param %3F should be \"?\""
    (ok (string= (barghest:decode-param "%3F") "?"))))

(deftest test-decode-param-colon
  (testing "Decoding param %3A should be \":\""
    (ok (string= (barghest:decode-param "%3A") ":"))))

(deftest test-decode-param-@
  (testing "Decoding param %40 should be \"@\""
    (ok (string= (barghest:decode-param "%40") "@"))))

(deftest test-decode-param-=
  (testing "Decoding param %3D should be \"=\""
    (ok (string= (barghest:decode-param "%3D") "="))))

(deftest test-decode-param-&
  (testing "Decoding param %26 should be \"&\""
    (ok (string= (barghest:decode-param "%26") "&"))))

(deftest test-decode-param-$
  (testing "Decoding param %24 should be \"$\""
    (ok (string= (barghest:decode-param "%24") "$"))))

(deftest test-decode-param-text
  (testing "Decoding param foo should be \"foo\""
    (ok (string= (barghest:decode-param "foo") "foo"))))

(deftest test-decode-param-with-spaces
  (testing "Decoding param foo+bar should be \"foo bar\""
    (ok (string= (barghest:decode-param "foo+bar") "foo bar"))))

(deftest test-decode-param-with-spaces
  (testing "Decoding param foo+bar should be \"foo bar\""
    (ok (string= (barghest:decode-param "foo+bar") "foo bar"))))

(deftest test-decode-param-text-with-code
  (testing "Decoding param foo%3F should be \"foo?\""
    (ok (string= (barghest:decode-param "foo%3F") "foo?"))))
