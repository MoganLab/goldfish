;; make-char-literal
;; 创建一个字符字面量记录，保存源码文本与解析后的字符值。
;;
;; 语法
;; ----
;; (make-char-literal [source] [value])
;;
;; 参数
;; ----
;; source : string? 可选，默认值为 ""
;; 字面量的源码文本。
;;
;; value : char? 可选，默认值为 #\space
;; 字面量解析后的字符值。
;;
;; 返回值
;; ----
;; char-literal?
;; 新创建的字符字面量记录。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((lit (make-char-literal "#\\a" #\a)))
  (check (char-literal? lit) => #t)
  (check (char-literal-source lit) => "#\\a")
  (check (char-literal-value lit) => #\a)
) ;let

;; 测试默认参数
(check (char-literal-source (make-char-literal)) => "")
(check (char-literal-value (make-char-literal)) => #\space)

;; 测试非法参数抛出 value-error
(check-catch 'value-error (make-char-literal 42))
(check-catch 'value-error (make-char-literal "src" "not-a-char"))

(check-report)
