;; render-container
;; 渲染容器节点（引用、Alert、列表）为行列表。
;;
;; 语法
;; ----
;; (render-container node render-blocks)
;;
;; 参数
;; ----
;; node : pair
;; 容器节点。
;; render-blocks : procedure
;; 块级渲染回调，用于递归渲染容器内子块。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表。
;;
;; 错误处理
;; ----
;; 容器实现随 ST4 补齐，当前显式报 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown containers))
(import (liii cmark markdown blocks))

(check-set-mode! 'report-failed)

(check-true (procedure? render-container))

;; 骨架阶段：容器节点显式报错
(check-catch 'md-stem-error
  (render-container '(blockquote (p "引用")) render-blocks)
) ;check-catch
(check-catch 'md-stem-error
  (render-container '(bullet-list tight (item)) render-blocks)
) ;check-catch

(check-report)
