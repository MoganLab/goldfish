(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; newline-node?
;; 判断 node 是否为换行标记节点：tag-name 为 *newline* 且唯一子节点为数字 atom。
;;
;; 语法
;; ----
;; (newline-node? node)
;;
;; 参数
;; ----
;; node : any?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：*newline* env 且子为数字 atom 时为 #t
(let ((node (make-env :tag-name "*newline*"
              :children (vector (make-atom :value 2))
            ) ;make-env
       ) ;node
     ) ;
  (check (newline-node? node) => #t)
) ;let

;; 测试：子为字符串 atom 时为 #f
(let ((node (make-env :tag-name "*newline*"
              :children (vector (make-atom :value "x"))
            ) ;make-env
       ) ;node
     ) ;
  (check (newline-node? node) => #f)
) ;let

;; 测试：普通 env 为 #f
(check (newline-node? (make-env :tag-name "define")) => #f)

(check-report)
