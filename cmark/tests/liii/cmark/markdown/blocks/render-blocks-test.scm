;; render-blocks
;; 把已校验的块节点列表渲染为按逻辑行组织的字符串列表。
;;
;; 语法
;; ----
;; (render-blocks nodes)
;;
;; 参数
;; ----
;; nodes : list
;; 块节点列表。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表；兄弟块间空行由文档层统一插入。
;;
;; 错误处理
;; ----
;; 节点非法或尚未支持时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown blocks))

(check-set-mode! 'report-failed)

(check-true (procedure? render-blocks))

;; 空块列表
(check (render-blocks '()) => '())

;; 非法节点先经校验报错
(check-catch 'md-stem-error (render-blocks '(42)))
(check-catch 'md-stem-error (render-blocks '((unknown-block "x"))))

;; 合法但尚未实现的节点显式报 unsupported，不返回空字符串
(check-catch 'md-stem-error (render-blocks '((p "正文"))))

(check-report)
