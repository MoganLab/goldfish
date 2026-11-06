;; uri?
;; 判断字符串是否为绝对 URI。
;;
;; 语法
;; ----
;; (uri? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的绝对 URI 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 绝对 URI 必须带 scheme，形如 scheme:path；
;; 相对引用不带 scheme，因此不属于绝对 URI。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-uri))

(check-set-mode! 'report-failed)

;; 合法绝对 URI。

(check-true (uri? "http://example.com"))
(check-true (uri? "https://example.com/a?b=c#d"))
(check-true (uri? "urn:example:resource"))
(check-true (uri? "mailto:user@example.com"))
(check-true (uri? "a+b-c.d:e"))
(check-true (uri? "http://foo.bar/?baz=qux#quux"))
(check-true (uri? "http://foo.com/blah_(wikipedia)_blah#cite-1"))
(check-true (uri? "http://foo.bar/?q=Test%20URL-encoded%20stuff"))
(check-true (uri? "http://223.255.255.254"))
(check-true (uri? "ftp://ftp.is.co.za/rfc/rfc1808.txt"))
(check-true (uri? "ldap://[2001:db8::7]/c=GB?objectClass?one"))
(check-true (uri? "mailto:John.Doe@example.com"))
(check-true (uri? "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"))

;; 相对引用不带 scheme。

(check-false (uri? "/relative"))
(check-false (uri? "example.com"))
(check-false (uri? "relative/path"))

;; scheme 不合法。

(check-false (uri? "://x"))
(check-false (uri? "1http://example.com"))
(check-false (uri? "//foo.bar/?baz=qux#quux"))
(check-false (uri? "/abc"))
(check-false (uri? "abc"))
(check-false (uri? "http:// shouldfail.com"))
(check-false (uri? ":// should fail"))
(check-false (uri? "bar,baz:foo"))
(check-false (uri? "https://[@example.org/test.txt"))
(check-false (uri? "https://example.org/foo bar.txt"))
(check-false (uri? "http://example.com/%6G"))
(check-false (uri? "http://example.com/%"))
(check-false (uri? "ht_tp://example.com"))
(check-false (uri? "http://example.com:abc/path"))

(check-report)
