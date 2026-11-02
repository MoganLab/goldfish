;; lib-exports
;; 从库解析信息 alist 中提取公开导出的符号列表。
;;
;; 语法
;; ----
;; (lib-exports info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; list of symbol
;; 导出的符号列表。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 获取导出符号列表
(let* ((info '((exports f1 f2))) (exps (lib-exports info)))
  (check exps => '(f1 f2))
) ;let*

(check-report)
