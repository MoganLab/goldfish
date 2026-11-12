;; all-in-one-default-chains
;; 获取内置默认 all-in-one 链定义列表。
;;
;; 语法
;; ----
;; (all-in-one-default-chains)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list of <chain-def>

(import (scheme base)
  (liii check)
  (liii pi code all-in-one provider)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(let ((chains (all-in-one-default-chains)))
  (check-true (pair? chains))
  (check (length chains) => 2)
  (check (chain-def-id (car chains)) => "coding-fast")
  (check (chain-def-id (cadr chains)) => "coding-high-quality")
) ;let

(check-report)
