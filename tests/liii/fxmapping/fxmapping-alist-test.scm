(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping->alist
;; 转换为关联列表（升序）。
;;
;; 语法
;; ----
;; (fxmapping->alist fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按键升序排列的关联列表 ((key . value) ...)。
;;
(check (fxmapping->alist (fxmapping 0 'a 1 'b))
  =>
  '((0 . a) (1 . b))
) ;check
(check (fxmapping->alist (fxmapping))
  =>
  '()
) ;check

(check-report)
