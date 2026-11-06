;; json-pointer?
;; 判断字符串是否为 JSON Pointer。
;;
;; 语法
;; ----
;; (json-pointer? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的 JSON Pointer 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; JSON Pointer 是空串，或由 / 分隔的引用记号序列。
;; 引用记号中的 ~ 只能写成 ~0 或 ~1。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-uri))

(check-set-mode! 'report-failed)

;; 合法 JSON Pointer。

(check-true (json-pointer? ""))
(check-true (json-pointer? "/"))
(check-true (json-pointer? "/foo"))
(check-true (json-pointer? "/foo/0"))
(check-true (json-pointer? "/foo/bar/baz"))
(check-true (json-pointer? "/a~1b"))
(check-true (json-pointer? "/a~0b"))

;; 必须以 / 开头。

(check-false (json-pointer? "foo"))
(check-false (json-pointer? "foo/bar"))

;; ~ 只能写成 ~0 或 ~1。

(check-false (json-pointer? "/a~2b"))
(check-false (json-pointer? "/foo~"))

(check-report)
