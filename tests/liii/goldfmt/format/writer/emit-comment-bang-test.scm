(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; emit-comment!
;; 将注释节点写入 writer，返回带定位信息的新 env。
;;
;; 语法
;; ----
;; (emit-comment! node writer column)
;;
;; 返回值
;; ------
;; env?

;; 测试：注释内容输出为 ;; 形式
(let* ((w (make-writer 0))
       (node (make-env :tag-name "*comment*"
                :children (vector (make-atom :value "note"))
              ) ;make-env
             ) ;node
       (new-node (emit-comment! node w 0)))
  (check (writer-result w) => ";; note")
  (check (env? new-node) => #t)
) ;let

;; 测试：空注释
(let* ((w (make-writer 0))
       (node (make-env :tag-name "*comment*"
                :children (vector (make-atom :value ""))
              ) ;make-env
             ) ;node
       (new-node (emit-comment! node w 0)))
  (check (writer-result w) => ";;")
) ;let

(check-report)
