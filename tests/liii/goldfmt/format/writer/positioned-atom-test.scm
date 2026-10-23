(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; positioned-atom / positioned-env
;; 为节点生成带缩进与行号定位信息的拷贝。

;; 测试：positioned-atom 保留 value 并带定位
(let* ((node (make-atom :value 7))
       (p (positioned-atom node 4 1 3)))
  (check (atom-value p) => 7)
  (check (atom-indent p) => 4)
  (check (atom-left-line p) => 1)
  (check (atom-right-line p) => 3)
) ;let*

;; 测试：positioned-env 保留 tag-name 与 children
(let* ((children (vector (make-atom :value 1)))
       (node (make-env :tag-name "+" :children children :value '(1)))
       (p (positioned-env node 2 children 1 5)))
  (check (env-tag-name p) => "+")
  (check (env-indent p) => 2)
  (check (env-left-line p) => 1)
  (check (env-right-line p) => 5)
) ;let*

(check-report)
