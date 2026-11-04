;; try-inline
;; 尝试将 node 按单行形式输出。
;;
;; 语法
;; ----
;; (try-inline node)
;;
;; 参数
;; ----
;; node : env? 或 atom?
;; 待尝试内联的节点。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 可以内联时返回单行候选文本，否则返回 #f。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

;; 测试：atom 节点返回单行文本
(check (try-inline (scan 'x)) => "x")

;; 测试：短的 env 节点返回单行文本
(check (try-inline (scan '(+ 1 2))) => "(+ 1 2)")

;; 测试：注释节点可以内联
(check (try-inline (scan (list '*comment* "note"))) => #t)

;; 测试：换行节点不能内联
(check (try-inline (scan '(*newline* 1))) => #f)

;; 测试：过长的 env 节点不能内联
(let ((node (scan '(+ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
            ) ;scan
      ) ;node
     ) ;
  (check (try-inline node) => #f)
) ;let

;; 测试：包含注释的 env 不能内联
(check
  (try-inline (scan (list 'begin (list '*comment* "note") 'x)))
  =>
  #f
) ;check

(check-report)
