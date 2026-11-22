;; render-block
;; 渲染单个已校验块节点为行列表。
;;
;; 语法
;; ----
;; (render-block node)
;;
;; 参数
;; ----
;; node : pair
;; 块节点。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表。
;;
;; 错误处理
;; ----
;; 节点非法或尚未支持时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown blocks))

(check-set-mode! 'report-failed)

(check-true (procedure? render-block))

(check (render-block '(p "正文")) => '("正文"))
(check (render-block '(h2 "标题")) => '("## 标题"))
(check (render-block '(thematic-break)) => '("---"))
(check-catch 'md-stem-error (render-block '(unknown-block "x")))

(check-report)
