;; make-raw-string-literal
;; 创建一个原始字符串字面量记录，保存源码文本与解析后的值。
;;
;; 语法
;; ----
;; (make-raw-string-literal [source] [value])
;;
;; 参数
;; ----
;; source : string? 可选，默认值为 ""
;; 字面量的源码文本。
;;
;; value : string? 可选，默认值为 ""
;; 字面量解析后的字符串值。
;;
;; 返回值
;; ----
;; raw-string-literal?
;; 新创建的原始字符串字面量记录。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((lit (make-raw-string-literal "#\"abc\"" "abc")))
  (check (raw-string-literal? lit) => #t)
  (check (raw-string-literal-source lit) => "#\"abc\"")
  (check (raw-string-literal-value lit) => "abc")
) ;let

;; 测试默认参数
(check (raw-string-literal-source (make-raw-string-literal)) => "")
(check (raw-string-literal-value (make-raw-string-literal)) => "")

;; 测试非法参数抛出 value-error
(check-catch 'value-error (make-raw-string-literal 42))
(check-catch 'value-error (make-raw-string-literal "src" 42))

(check-report)
