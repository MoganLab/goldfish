(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-size
;; 获取映射的大小（键值对数量）。
;;
;; 语法
;; ----
;; (fxmapping-size fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回 fxmap 中键值对的数量。
;;
(check (fxmapping-size (fxmapping))
  =>
  0
) ;check
(check (fxmapping-size (fxmapping 0 'a))
  =>
  1
) ;check
(check (fxmapping-size (fxmapping 0 'a 1 'b 2 'c)
       ) ;fxmapping-size
  =>
  3
) ;check

(check-report)
