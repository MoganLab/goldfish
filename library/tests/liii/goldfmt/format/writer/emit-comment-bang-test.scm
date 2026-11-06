;; emit-comment!
;; 将注释节点写入 writer，返回带定位信息的新 env。
;;
;; 语法
;; ----
;; (emit-comment! node writer column)
;;
;; 参数
;; ----
;; node : env?
;; 注释节点，tag-name 为 "*comment*"。
;;
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; column : integer?
;; 注释的起始列。

;; 返回值
;; ------
;; env?

(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record))

(check-set-mode! 'report-failed)


;; 测试：注释内容输出为 ;; 形式
(let* ((w (make-writer 0))
       (node (make-env :tag-name "*comment*" :children (vector (make-atom :value "note")))
       ) ;node
       (new-node (emit-comment! node w 0))
      ) ;
  (check (writer-result w) => ";; note")
  (check (env? new-node) => #t)
) ;let*

;; 测试：空注释
(let* ((w (make-writer 0))
       (node (make-env :tag-name "*comment*" :children (vector (make-atom :value ""))))
       (new-node (emit-comment! node w 0))
      ) ;
  (check (writer-result w) => ";;")
) ;let*

(check-report)
