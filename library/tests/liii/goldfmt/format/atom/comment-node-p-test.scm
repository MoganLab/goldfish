;; comment-node?
;; 判断 node 是否为注释节点：tag-name 为一次性 nonce 注释标记
;; 且唯一子节点为字符串 atom。
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

(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record) (liii goldfmt marker))

(check-set-mode! 'report-failed)


;; 测试：nonce 注释标记 env 且子为字符串 atom 时为 #t
(let ((node (make-env :tag-name %comment-marker-tag-name
               :children (vector (make-atom :value "note"))
             ) ;make-env
      ) ;node
     ) ;
  (check (comment-node? node) => #t)
) ;let

;; 测试：子为 number atom 时为 #f
(let ((node (make-env :tag-name %comment-marker-tag-name
               :children (vector (make-atom :value 1))
             ) ;make-env
      ) ;node
     ) ;
  (check (comment-node? node) => #f)
) ;let

;; 测试：用户写的 *comment* tag 不是注释节点
(let ((node (make-env :tag-name "*comment*"
               :children (vector (make-atom :value "note"))
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
