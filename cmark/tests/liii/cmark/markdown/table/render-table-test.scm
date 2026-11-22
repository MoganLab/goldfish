;; render-table
;; 渲染表格节点为行列表。
;;
;; 语法
;; ----
;; (render-table node)
;;
;; 参数
;; ----
;; node : pair
;; (table ...) 节点。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表。
;;
;; 错误处理
;; ----
;; 表格实现随 ST5 补齐，当前显式报 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown table))

(check-set-mode! 'report-failed)

(check-true (procedure? render-table))

;; 骨架阶段：表格节点显式报错
(check-catch 'md-stem-error
  (render-table '(table (align default) (thead (tr (th "a"))) (tbody)))
) ;check-catch

(check-report)
