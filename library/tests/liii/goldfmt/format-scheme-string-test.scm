;; format-scheme-string
;; format-string 的别名入口。
;;
;; 语法
;; ----
;; (format-scheme-string source)
;;
;; 参数
;; ----
;; source : string
;; 待格式化的 Scheme 源码文本。
;;
;; 返回值
;; ----
;; string
;; 格式化后的 Scheme 源码文本。
;;
;; 说明
;; ----
;; 与 (liii goldfmt format) 的 format-string 行为完全一致，
;; 用于按 Scheme 语义显式格式化源码。

(import (liii check) (liii goldfmt))

(check-set-mode! 'report-failed)

;; 与 format-string 结果一致
(check (format-scheme-string "(define (add a b) (+ a b))")
  =>
  "(define (add a b)\n  (+ a b)\n) ;define\n"
) ;check

(check (format-scheme-string "(let ((x 1) (y 2)) (+ x y))")
  =>
  "(let ((x 1) (y 2))\n  (+ x y)\n) ;let\n"
) ;check

(check-report)
