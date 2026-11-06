;; http-normalize-string-alist
;; 规范化字符串关联列表，键与值支持标量并统一转为字符串。
;;
;; 语法
;; ----
;; (http-normalize-string-alist who field entries)
;;
;; 参数
;; ----
;; who     : string?  调用方函数名称，用于错误信息前缀。
;; field   : string?  参数字段名称。
;; entries : alist?   键值对列表，键与值支持字符串、符号或数字。
;;
;; 返回值
;; ----
;; alist?
;; 键与值均已转为字符串的新关联列表。
;;
;; 错误处理
;; ----
;; type-error
;; entries 不是关联列表，或某一项不是键值对时抛出。

(import (liii check) (liii http common))

(check-set-mode! 'report-failed)

;; 空列表返回空列表
(check (http-normalize-string-alist "test" "params" '()) => '())

;; 字符串键值对原样保留
(check (http-normalize-string-alist "test" "params" '(("key" . "value"))) =>
  '(("key" . "value"))
) ;check

;; 符号与数字键值统一转为字符串
(check (http-normalize-string-alist "test" "params" '((key . 42))) => '(("key" . "42")))

;; 多个条目全部规范化
(check (http-normalize-string-alist "test" "headers" '(("a" . "1") (b . 2))) =>
  '(("a" . "1") ("b" . "2"))
) ;check

;; 非关联列表抛出 type-error
(check-catch 'type-error (http-normalize-string-alist "test" "params" "not-alist"))
(check-catch 'type-error (http-normalize-string-alist "test" "params" 42))
(check-catch 'type-error (http-normalize-string-alist "test" "params" (list "a" "b")))

;; 条目不是键值对抛出 type-error
(check-catch 'type-error (http-normalize-string-alist "test" "params" '(("k" "v"))))
(check-catch 'type-error (http-normalize-string-alist "test" "params" '(("k" . ("v1" . "v2")))))
(check-catch 'type-error (http-normalize-string-alist "test" "params" '(("k" . ()))) )
(check-catch 'type-error (http-normalize-string-alist "test" "params" '(("k" . #t))))

(check-report)
