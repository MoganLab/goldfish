(import (liii json schema)
  (liii json schema format)
  (liii json)
  (liii string)
  (liii check)
  (scheme base)
) ;import
(check-set-mode! 'report-failed)

(define email-schema '(("format" . "email")))

(define other-schema '(("format" . "not-a-known-format")))

;; A format checker is a procedure from a string to a boolean.

(define has-at (lambda (value) (string-contains value "@")))

;; The checker alist must be built with cons: a quoted (name . (lambda ...))
;; would read as a proper list instead of a dotted pair.

(define email-checkers (list (cons 'email has-at)))

;; 1. 默认配置下 format 只作为注解，任何字符串都不因格式而失败。
(check-true (json-schema-valid? "2962" email-schema))
(check-true (json-schema-valid? "not-an-email-at-all" email-schema))
(check-true (json-schema-valid? "2962" other-schema))

;; 2. 非字符串实例不参与格式检查。
(check-true (json-schema-valid? 12 email-schema))
(check-true (json-schema-valid? 13.7 email-schema))
(check-true (json-schema-valid? 'null email-schema))
(check-true (json-schema-valid? #t email-schema))
(check-true (json-schema-valid? '(("a" . 1)) email-schema))
(check-true (json-schema-valid? '#(1 2) email-schema))

;; 3. formats 选项开启后只有列出的格式参与断言。
(check-false
  (json-schema-valid? "2962"
    email-schema
    `((formats email) (format-checkers . ,email-checkers))
  ) ;json-schema-valid?
) ;check-false
(check-true
  (json-schema-valid? "2962"
    email-schema
    `((formats hostname) (format-checkers . ,email-checkers))
  ) ;json-schema-valid?
) ;check-true

;; 4. format-checkers 注册的自定义检查器按名称生效。
(check-true
  (json-schema-valid? "user@example.com"
    email-schema
    `((formats email) (format-checkers . ,email-checkers))
  ) ;json-schema-valid?
) ;check-true

;; 5. formats 为 #t 时断言所有已知格式。
(check-false
  (json-schema-valid? "2962"
    email-schema
    `((formats . #t) (format-checkers . ,email-checkers))
  ) ;json-schema-valid?
) ;check-false

;; 6. 未知格式名不产生错误，只作为注解。
(check-true
  (json-schema-valid? "anything" other-schema `((formats . #t)))
) ;check-true

;; 7. 格式错误的关键字与路径。

(define format-error
  (car
    (json-schema-errors "2962"
      email-schema
      `((formats email) (format-checkers . ,email-checkers))
    ) ;json-schema-errors
  ) ;car
) ;define
(check (json-schema-error-keyword format-error) => "format")
(check (json-schema-error-path format-error) => '())
(check (json-schema-error-schema-path format-error) => '("format"))

;; 8. 嵌套实例的格式错误路径逐段保留。

(define nested-format-error
  (car
    (json-schema-errors '(("a" . "2962"))
      '(("properties" ("a" ("format" . "email"))))
      `((formats email) (format-checkers . ,email-checkers))
    ) ;json-schema-errors
  ) ;car
) ;define
(check (json-schema-error-keyword nested-format-error) => "format")
(check (json-schema-error-path nested-format-error) => '("a"))

;; 9. 选项本身必须合法。
(check-catch 'json-schema-options-error
  (json-schema-valid? "2962" email-schema `((formats . "email")))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? "2962" email-schema `((formats email . #t)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? "2962"
    email-schema
    '((format-checkers list (cons 'email "not-a-procedure")))
  ) ;json-schema-valid?
) ;check-catch

;; 10. 选项在多个校验之间互相隔离。

(define format-validator
  (make-json-schema-validator email-schema
    `((formats email) (format-checkers . ,email-checkers))
  ) ;make-json-schema-validator
) ;define
(check-false (json-schema-validator-valid? format-validator "2962"))
(check-true (json-schema-validator-valid? format-validator "user@example.com"))
(check-true (json-schema-valid? "2962" email-schema))

;; ---- built-in checkers (task 036) --------------------------------------

(define (checker name)
  (cdr (assoc name (json-schema-builtin-format-checkers)))
) ;define

(define (assert-format name value expected)
  (if expected
    (check-true ((checker name) value))
    (check-false ((checker name) value))
  ) ;if
) ;define

;; ipv4: four dotted decimal octets only.
(assert-format 'ipv4 "192.168.0.1" #t)
(assert-format 'ipv4 "0.0.0.0" #t)
(assert-format 'ipv4 "255.255.255.255" #t)
(assert-format 'ipv4 "127.0.0.0.0.1" #f)
(assert-format 'ipv4 "256.256.256.256" #f)
(assert-format 'ipv4 "127.0" #f)
(assert-format 'ipv4 "0x7f000001" #f)
(assert-format 'ipv4 "2130706433" #f)
(assert-format 'ipv4 " 192.168.0.1" #f)
(assert-format 'ipv4 "192.168.0.1 " #f)
(assert-format 'ipv4 "192.168.1.0/24" #f)
(assert-format 'ipv4 "192.168..1" #f)
(assert-format 'ipv4 "1e2.0.0.1" #f)
(assert-format 'ipv4 "192.168.0.1:80" #f)

;; ipv6: :: compression and embedded IPv4 are accepted, zone ids are not.
(assert-format 'ipv6 "::1" #t)
(assert-format 'ipv6 "::" #t)
(assert-format 'ipv6 "1:2:3:4:5:6:7:8" #t)
(assert-format 'ipv6 "1::d6:192.168.0.1" #t)
(assert-format 'ipv6 "::ffff:192.168.0.1" #t)
(assert-format 'ipv6 "12345::" #f)
(assert-format 'ipv6 "::abcef" #f)
(assert-format 'ipv6 "::laptop" #f)
(assert-format 'ipv6 ":2:3:4:5:6:7:8" #f)
(assert-format 'ipv6 "1:2:3:4:5:6:7" #f)
(assert-format 'ipv6 "1::d6::42" #f)
(assert-format 'ipv6 "1::2:192.168.256.1" #f)
(assert-format 'ipv6 "1:2:3:4:5:::8" #f)
(assert-format 'ipv6 "fe80::/64" #f)
(assert-format 'ipv6 "fe80::a%eth1" #f)

;; email: dot atom or quoted local part, hostname or IP literal domain.
(assert-format 'email "joe.bloggs@example.com" #t)
(assert-format 'email "te~st@example.com" #t)
(assert-format 'email "~test@example.com" #t)
(assert-format 'email "\"joe bloggs\"@example.com" #t)
(assert-format 'email "joe.bloggs@[127.0.0.1]" #t)
(assert-format 'email "joe.bloggs@[IPv6:::1]" #t)
(assert-format 'email "2962" #f)
(assert-format 'email ".test@example.com" #f)
(assert-format 'email "te..st@example.com" #f)
(assert-format 'email "joe.bloggs@invalid=domain.com" #f)
(assert-format 'email "joe.bloggs@[127.0.0.300]" #f)
(assert-format 'email "@example.com" #f)
(assert-format 'email "joe.bloggs@" #f)
(assert-format 'email "joe bloggs@example.com" #f)

;; idn-email additionally accepts non ASCII characters.
(assert-format 'idn-email "user@example.com" #t)
(assert-format 'idn-email "2962" #f)

;; hostname: RFC 1123 label rules.
(assert-format 'hostname "www.example.com" #t)
(assert-format 'hostname "h0stn4me" #t)
(assert-format 'hostname "1host" #t)
(assert-format 'hostname "host-name" #t)
(assert-format 'hostname "" #f)
(assert-format 'hostname "." #f)
(assert-format 'hostname ".example" #f)
(assert-format 'hostname "example." #f)
(assert-format 'hostname "-hostname" #f)
(assert-format 'hostname "hostname-" #f)
(assert-format 'hostname "host_name" #f)

;; The built-in checkers are reachable through the formats option.
(check-true (json-schema-valid? "192.168.0.1" '(("format" . "ipv4")) '((formats ipv4)))
) ;check-true
(check-false (json-schema-valid? "256.256.256.256" '(("format" . "ipv4")) '((formats ipv4)))
) ;check-false
(check-true (json-schema-valid? "user@example.com"
              '(("format" . "email"))
              '((formats email))
            ) ;json-schema-valid?
) ;check-true

;; ---- built-in checkers (task 037) --------------------------------------

;; date: strict YYYY-MM-DD with real calendar days.
(assert-format 'date "1963-06-19" #t)
(assert-format 'date "2020-01-31" #t)
(assert-format 'date "2020-02-29" #t)
(assert-format 'date "0400-02-29" #t)
(assert-format 'date "0001-01-01" #t)
(assert-format 'date "2020-01-32" #f)
(assert-format 'date "2020-02-30" #f)
(assert-format 'date "2021-02-29" #f)
(assert-format 'date "0100-02-29" #f)
(assert-format 'date "2100-02-29" #f)
(assert-format 'date "06/19/1963" #f)
(assert-format 'date "1998-1-20" #f)
(assert-format 'date "20230328" #f)
(assert-format 'date "2020-01-01Z" #f)
(assert-format 'date "" #f)

;; time: HH:MM:SS with optional fraction and time zone.
(assert-format 'time "08:30:06Z" #t)
(assert-format 'time "08:30:06z" #t)
(assert-format 'time "23:20:50.52Z" #t)
(assert-format 'time "08:30:06+00:20" #t)
(assert-format 'time "12:00:00" #f)
(assert-format 'time "8:3:6Z" #f)
(assert-format 'time "24:00:00Z" #f)
(assert-format 'time "00:60:00Z" #f)
(assert-format 'time "01:02:03+24:00" #f)
(assert-format 'time "01:02:03+00:60" #f)
(assert-format 'time "01:02:03Z+00:30" #f)
(assert-format 'time "ab:cd:ef" #f)

;; time: a leap second is only valid at 23:59:60 UTC.
(assert-format 'time "23:59:60Z" #t)
(assert-format 'time "23:59:60+00:00" #t)
(assert-format 'time "01:29:60+01:30" #t)
(assert-format 'time "15:59:60-08:00" #t)
(assert-format 'time "22:59:60Z" #f)
(assert-format 'time "23:58:60Z" #f)
(assert-format 'time "23:59:60+01:00" #f)
(assert-format 'time "23:59:60-01:00" #f)

;; date-time combines a date and a time.
(assert-format 'date-time "1963-06-19T08:30:06.283185Z" #t)
(assert-format 'date-time "1963-06-19t08:30:06.283185z" #t)
(assert-format 'date-time "1937-01-01T12:00:27.87+00:20" #t)
(assert-format 'date-time "1998-12-31T23:59:60Z" #t)
(assert-format 'date-time "1990-02-31T15:59:59.123-08:00" #f)
(assert-format 'date-time "1963-6-19T08:30:06.283185Z" #f)
(assert-format 'date-time "1990-12-31T15:59:59-24:00" #f)

;; duration follows the ISO 8601 component order.
(assert-format 'duration "P4DT12H30M5S" #t)
(assert-format 'duration "P4Y" #t)
(assert-format 'duration "PT0S" #t)
(assert-format 'duration "PT36H" #t)
(assert-format 'duration "P2W" #t)
(assert-format 'duration "P1Y2M3DT4H5M6S" #t)
(assert-format 'duration "PT1D" #f)
(assert-format 'duration "P1YT" #f)
(assert-format 'duration "P2D1Y" #f)
(assert-format 'duration "P1D2H" #f)
(assert-format 'duration "P1Y2W" #f)
(assert-format 'duration "P1Y2D" #f)
(assert-format 'duration "PT1H2S" #f)
(assert-format 'duration "PT0.5S" #f)
(assert-format 'duration " P1D" #f)

;; The date and time checkers are reachable through the formats option.
(check-true (json-schema-valid? "1963-06-19" '(("format" . "date")) '((formats date)))
) ;check-true
(check-false (json-schema-valid? "1963-06-32" '(("format" . "date")) '((formats date)))
) ;check-false

;; ---- built-in checkers (task 038) --------------------------------------

;; uri: an absolute URI with a scheme and RFC 3986 characters.
(assert-format 'uri "http://foo.bar/?baz=qux#quux" #t)
(assert-format 'uri "http://foo.com/blah_(wikipedia)_blah#cite-1" #t)
(assert-format 'uri "http://foo.bar/?q=Test%20URL-encoded%20stuff" #t)
(assert-format 'uri "http://223.255.255.254" #t)
(assert-format 'uri "ftp://ftp.is.co.za/rfc/rfc1808.txt" #t)
(assert-format 'uri "ldap://[2001:db8::7]/c=GB?objectClass?one" #t)
(assert-format 'uri "mailto:John.Doe@example.com" #t)
(assert-format 'uri "urn:oasis:names:specification:docbook:dtd:xml:4.1.2" #t)
(assert-format 'uri "//foo.bar/?baz=qux#quux" #f)
(assert-format 'uri "/abc" #f)
(assert-format 'uri "abc" #f)
(assert-format 'uri "http:// shouldfail.com" #f)
(assert-format 'uri ":// should fail" #f)
(assert-format 'uri "bar,baz:foo" #f)
(assert-format 'uri "https://[@example.org/test.txt" #f)
(assert-format 'uri "https://example.org/foobar\\.txt" #f)
(assert-format 'uri "https://example.org/foobar\".txt" #f)
(assert-format 'uri "https://example.org/foobar{}.txt" #f)
(assert-format 'uri "https://example.org/foo bar.txt" #f)
(assert-format 'uri "http://example.com/%6G" #f)
(assert-format 'uri "http://example.com/%" #f)
(assert-format 'uri "1http://example.com" #f)
(assert-format 'uri "ht_tp://example.com" #f)
(assert-format 'uri "http://example.com:abc/path" #f)

;; uri-reference also accepts relative references.
(assert-format 'uri-reference "//foo.bar/?baz=qux#quux" #t)
(assert-format 'uri-reference "/abc" #t)
(assert-format 'uri-reference "abc" #t)
(assert-format 'uri-reference "#fragment" #t)
(assert-format 'uri-reference "#frag\\ment" #f)

;; json-pointer: RFC 6901 reference tokens.
(assert-format 'json-pointer "/foo/bar~0/baz~1/%a" #t)
(assert-format 'json-pointer "/foo//bar" #t)
(assert-format 'json-pointer "" #t)
(assert-format 'json-pointer "/" #t)
(assert-format 'json-pointer "/a~1b" #t)
(assert-format 'json-pointer "/foo/bar~" #f)

;; relative-json-pointer: a non-negative index plus an optional pointer.
(assert-format 'relative-json-pointer "1" #t)
(assert-format 'relative-json-pointer "0/foo/bar" #t)
(assert-format 'relative-json-pointer "0#" #t)
(assert-format 'relative-json-pointer "120/foo/bar" #t)
(assert-format 'relative-json-pointer "/foo/bar" #f)
(assert-format 'relative-json-pointer "-1/foo/bar" #f)
(assert-format 'relative-json-pointer "01/a" #f)
(assert-format 'relative-json-pointer "0##" #f)
(assert-format 'relative-json-pointer "" #f)

;; regex: the pattern must compile with the backend.
(assert-format 'regex "([abc])+\\s+$" #t)
(assert-format 'regex "^(abc]" #f)

;; The URI checkers are reachable through the formats option.
(check-true (json-schema-valid? "http://foo.bar/?baz=qux#quux"
              '(("format" . "uri"))
              '((formats uri))
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? "not a uri" '(("format" . "uri")) '((formats uri)))
) ;check-false

(check-report)
