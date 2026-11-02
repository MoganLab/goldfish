;; lib-line
;; 从库解析信息 alist 中提取 define-library 所在源码行号。
;;
;; 语法
;; ----
;; (lib-line info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; integer
;; 源码行号。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 获取行号
(let* ((info '((lib-name foo) (line . 42))) (line (lib-line info)))
  (check line => 42)
) ;let*

(check-report)
