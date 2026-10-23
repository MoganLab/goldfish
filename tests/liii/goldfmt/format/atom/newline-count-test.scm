(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; newline-count
;; 读取换行标记节点记录的空行数量。
;;
;; 语法
;; ----
;; (newline-count node)
;;
;; 参数
;; ----
;; node : newline-node? 为 #t 的节点
;;
;; 返回值
;; ------
;; number?
;; 子 atom 的数字值。

;; 测试：读取数字值
(let ((node (make-env :tag-name "*newline*"
              :children (vector (make-atom :value 3))
            ) ;make-env
       ) ;node
     ) ;
  (check (newline-count node) => 3)
) ;let

;; 测试：值为 1
(let ((node (make-env :tag-name "*newline*"
              :children (vector (make-atom :value 1))
            ) ;make-env
       ) ;node
     ) ;
  (check (newline-count node) => 1)
) ;let

(check-report)
