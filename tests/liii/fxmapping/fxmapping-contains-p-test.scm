(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-contains?
;; 检查映射是否包含指定键。
;;
;; 语法
;; ----
;; (fxmapping-contains? fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要检查的键。
;;
;; 返回值
;; -----
;; 如果 fxmap 包含 key，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-contains? (fxmapping 0 'a 1 'b)
              0
            ) ;fxmapping-contains?
) ;check-true
(check-true (fxmapping-contains? (fxmapping 0 'a 1 'b)
              1
            ) ;fxmapping-contains?
) ;check-true
(check-false (fxmapping-contains? (fxmapping 0 'a 1 'b)
               2
             ) ;fxmapping-contains?
) ;check-false

(check-report)
