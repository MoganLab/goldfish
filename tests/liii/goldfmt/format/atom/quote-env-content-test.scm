(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; quote-env-content
;; 读取 quote 表达式节点引用的内容（env-value 的第二个元素）。
;;
;; 语法
;; ----
;; (quote-env-content node)
;;
;; 参数
;; ----
;; node : quote-env? 为 #t 的节点
;;
;; 返回值
;; ------
;; any?
;; 引用的内容，value 结构不满足时返回空列表。

;; 测试：读取引用内容
(let ((node (make-env :tag-name "quote" :value (list 'quote '(a b)))))
  (check (quote-env-content node) => '(a b))
) ;let

;; 测试：value 为空列表时返回空列表
(check (quote-env-content (make-env :tag-name "quote")) => '())

(check-report)
