;; stem-format-nodes
;; 在 stem 模式下格式化一组 node 树。
;;
;; 语法
;; ----
;; (stem-format-nodes nodes)
;;
;; 参数
;; ----
;; nodes : vector?
;; 待格式化的 node 树集合。
;;
;; 返回值
;; ----
;; string?
;; 格式化后的文本。
;;
;; 说明
;; ----
;; stem 模式下 quote 与 quasiquote 形式不会被折叠，
;; 因此模板片段会原样保留。

(import (liii check) (liii goldfmt stem) (liii goldfmt scan) (srfi srfi-13))

(check-set-mode! 'report-failed)

;; 测试：unquote 形式不会被折叠
(check-true
  (string-contains
    (stem-format-nodes (call-with-stem-mode (lambda () (scan-string "(document (title (unquote t)))")))
    ) ;stem-format-nodes
    "(unquote t)"
  ) ;string-contains
) ;check-true

;; 测试：不会出现 quasiquote 折叠后的结果
(check-false
  (string-contains
    (stem-format-nodes (call-with-stem-mode (lambda () (scan-string "(document (title (unquote t)))")))
    ) ;stem-format-nodes
    ",t"
  ) ;string-contains
) ;check-false

(check-report)
