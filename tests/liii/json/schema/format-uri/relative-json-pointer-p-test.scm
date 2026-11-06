;; relative-json-pointer?
;; 判断字符串是否为相对 JSON Pointer。
;;
;; 语法
;; ----
;; (relative-json-pointer? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的相对 JSON Pointer 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 相对 JSON Pointer 由非负整数下标、可选的 # 与可选的 JSON Pointer 组成，
;; 下标不允许前导零。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-uri))

(check-set-mode! 'report-failed)

;; 合法相对 JSON Pointer。

(check-true (relative-json-pointer? "0"))
(check-true (relative-json-pointer? "1"))
(check-true (relative-json-pointer? "12"))
(check-true (relative-json-pointer? "0#"))
(check-true (relative-json-pointer? "1/foo"))
(check-true (relative-json-pointer? "2#/"))
(check-true (relative-json-pointer? "0/a~1b"))
(check-true (relative-json-pointer? "0/foo/bar"))
(check-true (relative-json-pointer? "120/foo/bar"))

;; 必须以数字开头。

(check-false (relative-json-pointer? ""))
(check-false (relative-json-pointer? "abc"))

;; 下标不允许前导零。

(check-false (relative-json-pointer? "01"))

;; 不允许负数下标。

(check-false (relative-json-pointer? "-1"))
(check-false (relative-json-pointer? "/foo/bar"))
(check-false (relative-json-pointer? "0##"))

(check-report)
