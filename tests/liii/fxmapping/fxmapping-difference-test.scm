(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-difference
;; 差集操作。
;;
;; 语法
;; ----
;; (fxmapping-difference fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; fxmap1 : fxmapping
;; 基础映射。
;;
;; fxmap2, ... : fxmapping
;; 要减去的映射。
;;
;; 返回值
;; -----
;; 返回 fxmap1 中不包含在 fxmap2... 中的键的新 fxmapping。
;;
(let ((diff (fxmapping-difference (fxmapping 0 'a 1 'b 2 'c)
              (fxmapping 1 'x 3 'y)
            ) ;fxmapping-difference
      ) ;diff
     ) ;
  (check-true (fxmapping-contains? diff 0)
  ) ;check-true
  (check-false (fxmapping-contains? diff 1)
  ) ;check-false
  (check-true (fxmapping-contains? diff 2)
  ) ;check-true
) ;let

(check-report)
