;; format-reader-pair-fill
;; 以 fill 布局格式化表单。
;;
;; 语法
;; ----
;; (format-reader-pair-fill datum indent fmt-inline)
;;
;; 参数
;; ----
;; datum : list?
;; 待格式化的表单。
;;
;; indent : integer?
;; 表单的起始缩进列。
;;
;; fmt-inline : procedure?
;; 内联格式化过程。
;;
;; 返回值
;; ----
;; string?
;; fill 布局下的格式化文本。

(import (liii check) (liii goldfmt format datum) (liii goldfmt format reader))

(check-set-mode! 'report-failed)

(check (format-reader-pair-fill '(a b c d e) 0 format-reader-datum-inline)
  =>
  "(a b c d e)"
) ;check

(check-report)
