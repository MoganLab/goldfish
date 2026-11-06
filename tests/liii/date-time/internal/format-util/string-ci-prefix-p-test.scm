;; string-ci-prefix?
;; 判断 prefix 是否与 str 从 pos 起的片段大小写不敏感前缀匹配。
;;
;; 语法
;; ----
;; (string-ci-prefix? str pos prefix)
;;
;; 参数
;; ----
;; str : string? 被匹配的输入串。
;; pos : integer? 起始位置。
;; prefix : string? 待匹配前缀。
;;
;; 返回值
;; ----
;; boolean? 匹配返回 #t，越界或不等返回 #f。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check-true (string-ci-prefix? "Hello" 0 "he"))
(check-true (string-ci-prefix? "HELLO" 0 "hello"))
(check-true (string-ci-prefix? "Hello" 1 "el"))
(check-false (string-ci-prefix? "Hello" 0 "world"))
(check-false (string-ci-prefix? "Hi" 0 "Hello"))

(check-report)
