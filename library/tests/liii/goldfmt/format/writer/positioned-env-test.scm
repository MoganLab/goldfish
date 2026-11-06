;; positioned-env
;; 为 env 节点生成带定位信息的拷贝。
;;
;; 语法
;; ----
;; (positioned-env node indent children left-line right-line)
;;
;; 参数
;; ----
;; node : env?
;; 原始 env 节点。
;;
;; indent : integer?
;; 节点左侧起始列。
;;
;; children : vector?
;; 已定位的子节点向量。
;;
;; left-line : integer?
;; 节点开始输出的行号。
;;
;; right-line : integer?
;; 节点完成输出的行号。
;;
;; 返回值
;; ----
;; env?
;; 带定位信息的新 env 节点。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

(let* ((children (vector (make-atom :value 1)))
       (node (make-env :tag-name "+" :depth 1 :children children :value '(1)))
       (p (positioned-env node 2 children 1 5))
      ) ;
  (check (env? p) => #t)
  (check (env-tag-name p) => "+")
  (check (env-depth p) => 1)
  (check (env-indent p) => 2)
  (check (env-left-line p) => 1)
  (check (env-right-line p) => 5)
  (check (env-children p) => children)
) ;let*

(check-report)
