(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; contains-comment?
;; 判断 node 树中是否包含注释节点。
;;
;; 语法
;; ----
;; (contains-comment? node)
;;
;; 参数
;; ----
;; node : atom? 或 env?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：注释节点本身返回 #t
(let ((node (make-env :tag-name "*comment*"
              :children (vector (make-atom :value "x"))
            ) ;make-env
       ) ;node
     ) ;
  (check (contains-comment? node) => #t)
) ;let

;; 测试：子节点含注释的 env 返回 #t
(let* ((comment (make-env :tag-name "*comment*"
                 :children (vector (make-atom :value "x"))
               ) ;make-env
              ) ;comment
      (node (make-env :tag-name "begin"
              :children (vector (make-atom :value 1) comment)
            ) ;make-env
       ) ;node
     ) ;
  (check (contains-comment? node) => #t)
) ;let*

;; 测试：不含注释的 env 返回 #f
(check (contains-comment? (make-env :tag-name "begin"
                             :children (vector (make-atom :value 1))
                           ) ;make-env
         ) ;check
  => #f
) ;check

;; 测试：atom 返回 #f
(check (contains-comment? (make-atom :value 1)) => #f)

(check-report)
