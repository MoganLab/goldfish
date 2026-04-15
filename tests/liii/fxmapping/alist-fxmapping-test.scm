(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; alist->fxmapping
;; 从关联列表（alist）创建整数映射。
;;
;; 语法
;; ----
;; (alist->fxmapping alist)
;;
;; 参数
;; ----
;; alist : list of pairs
;; 形如 ((key . value) ...) 的关联列表。
;;
;; 返回值
;; -----
;; 返回包含 alist 中所有键值对的新 fxmapping。
;; 如果存在重复键，后面的值会覆盖前面的值。
;;
(check (fxmapping-ref (alist->fxmapping '((0 . a) (1 . b)))
         0
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'a
) ;check
(check (fxmapping-ref (alist->fxmapping '((0 . a) (1 . b)))
         1
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'b
) ;check
(check-true (fxmapping-empty? (alist->fxmapping '())
            ) ;fxmapping-empty?
) ;check-true

(check-report)
