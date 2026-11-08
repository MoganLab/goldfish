;; maybe-escaped
;; 条件转义组合子：字符串满足谓词时才按 escaped 输出。
;;
;; 语法
;; ----
;; (maybe-escaped str pred [quot [esc [rename]]])
;;
;; 参数
;; ----
;; str : string?
;; 待输出的字符串。
;;
;; pred : (char? -> boolean?)
;; 逐字符判断的谓词，任一字符命中即触发转义。
;;
;; quot : char?
;; 可选，转义引号字符，默认 `#\"`。
;;
;; 返回值
;; ----
;; format?
;; 按需转义输出字符串的格式化组合子。
;;
;; 说明
;; ----
;; 未命中谓词时原样输出，命中时行为与 escaped 一致，
;; 常用于「含空白才加引号」的展示规则。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (scheme char) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (maybe-escaped "bob" char-whitespace?)) => "bob")
(check (show #f (maybe-escaped "hi, bob!" char-whitespace?)) => "\"hi, bob!\"")

(check-report)
