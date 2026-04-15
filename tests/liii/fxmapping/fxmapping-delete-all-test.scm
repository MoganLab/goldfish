(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-delete-all
;; 删除多个键。
;;
;; 语法
;; ----
;; (fxmapping-delete-all fxmap keys)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; keys : list of exact-integer
;; 要删除的键列表。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含 keys 中的任何键。
;;
(check-false (fxmapping-contains? (fxmapping-delete-all (fxmapping 0 'a 1 'b 2 'c)
                                    '(0 2)
                                  ) ;fxmapping-delete-all
               0
             ) ;fxmapping-contains?
) ;check-false
(check-true (fxmapping-contains? (fxmapping-delete-all (fxmapping 0 'a 1 'b 2 'c)
                                   '(0 2)
                                 ) ;fxmapping-delete-all
              1
            ) ;fxmapping-contains?
) ;check-true
(check-false (fxmapping-contains? (fxmapping-delete-all (fxmapping 0 'a 1 'b 2 'c)
                                    '(0 2)
                                  ) ;fxmapping-delete-all
               2
             ) ;fxmapping-contains?
) ;check-false

(check-report)
