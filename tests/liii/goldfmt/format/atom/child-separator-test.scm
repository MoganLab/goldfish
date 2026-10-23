(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; child-separator
;; 计算父节点第 child-index 个子节点之前应输出的分隔符：
;; 匿名列表首子无分隔符，其余为单个空格。
;;
;; 语法
;; ----
;; (child-separator parent child-index)
;;
;; 参数
;; ----
;; parent : env?
;; child-index : integer?
;;
;; 返回值
;; ------
;; string?
;; "" 或 " "。

;; 测试：匿名 env 的第 0 个子节点分隔符为空
(check (child-separator (make-env :tag-name "") 0) => "")

;; 测试：匿名 env 的第 1 个子节点分隔符为空格
(check (child-separator (make-env :tag-name "") 1) => " ")

;; 测试：带 tag 的 env 首子分隔符为空格
(check (child-separator (make-env :tag-name "define") 0) => " ")

(check-report)
