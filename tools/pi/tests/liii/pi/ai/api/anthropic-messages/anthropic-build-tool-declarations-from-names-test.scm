;; anthropic-build-tool-declarations-from-names
;; 依据启用的工具名列表构造 Anthropic API 的 tools 声明。
;;
;; 语法
;; ----
;; (anthropic-build-tool-declarations-from-names active-tool-names tool-resolver)
;;
;; 参数
;; ----
;; active-tool-names : list | #f
;; 允许模型调用的工具名列表。
;; tool-resolver : procedure
;; 工具名到工具定义的解析函数，返回 #f 表示该工具当前不可用。
;;
;; 返回值
;; ----
;; list
;; Anthropic API 的 tool 声明列表，解析失败的工具会被过滤。

(import (liii check)
  (liii pi ai api anthropic-messages)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

(let ((decls (anthropic-build-tool-declarations-from-names '("bash" "read")
               get-tool-definition
             ) ;anthropic-build-tool-declarations-from-names
      ) ;decls
     ) ;
  (check (length decls) => 2)
  (check (cdr (assoc "name" (car decls))) => "bash")
  (check (cdr (assoc "name" (cadr decls))) => "read")
) ;let

(check-report)
