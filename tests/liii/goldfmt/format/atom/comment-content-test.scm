(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; comment-content
;; 读取注释节点的文本内容。
;;
;; 语法
;; ----
;; (comment-content node)
;;
;; 参数
;; ----
;; node : comment-node? 为 #t 的节点
;;
;; 返回值
;; ------
;; string?
;; 子 atom 的字符串值。

;; 测试：读取注释文本
(let ((node (make-env :tag-name "*comment*"
              :children (vector (make-atom :value " 说明"))
            ) ;make-env
       ) ;node
     ) ;
  (check (comment-content node) => " 说明")
) ;let

;; 测试：空注释
(let ((node (make-env :tag-name "*comment*"
              :children (vector (make-atom :value ""))
            ) ;make-env
       ) ;node
     ) ;
  (check (comment-content node) => "")
) ;let

(check-report)
