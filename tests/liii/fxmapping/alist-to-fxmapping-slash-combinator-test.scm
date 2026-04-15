(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; alist->fxmapping/combinator
;; 从关联列表创建整数映射，使用自定义合并函数处理重复键。
;;
;; 语法
;; ----
;; (alist->fxmapping/combinator combiner alist)
;;
;; 参数
;; ----
;; combiner : procedure
;; 合并函数，接收三个参数：key、new-value、old-value，
;; 返回合并后的值。
;;
;; alist : list of pairs
;; 关联列表。
;;
;; 返回值
;; -----
;; 返回包含合并后键值对的新 fxmapping。
;;
(check (fxmapping-ref (alist->fxmapping/combinator (lambda (k new old) old)
                        '((0 . a) (0 . b))
                      ) ;alist->fxmapping/combinator
         0
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'a
) ;check
(check (fxmapping-ref (alist->fxmapping/combinator (lambda (k new old) new)
                        '((0 . a) (0 . b))
                      ) ;alist->fxmapping/combinator
         0
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'b
) ;check

(check-report)
