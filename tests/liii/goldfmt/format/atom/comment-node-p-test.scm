(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; comment-node?
;; 判断 node 是否为注释节点：tag-name 为 *comment* 且唯一子节点为字符串 atom。
;;
;; 语法
;; ----
;; (comment-node? node)
;;
;; 参数
;; ----
;; node : any?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：*comment* env 且子为字符串 atom 时为 #t
(let ((node (make-env :tag-name "*comment*"
              :children (vector (make-atom :value "note"))
            ) ;make-env
       ) ;node
     ) ;
  (check (comment-node? node) => #t)
) ;let

;; 测试：子为 number atom 时为 #f
(let ((node (make-env :tag-name "*comment*"
              :children (vector (make-atom :value 1))
            ) ;make-env
       ) ;node
     ) ;
  (check (comment-node? node) => #f)
) ;let

;; 测试：普通 env 为 #f
(check (comment-node? (make-env :tag-name "define")) => #f)

;; 测试：atom 为 #f
(check (comment-node? (make-atom :value "x")) => #f)

(check-report)
