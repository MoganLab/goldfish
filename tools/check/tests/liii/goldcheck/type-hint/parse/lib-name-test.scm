;; lib-name
;; 从库解析信息 alist 中提取库名符号列表。
;;
;; 语法
;; ----
;; (lib-name info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; list of symbol
;; 库名符号列表（如 '(liii clean)）。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 从已构造的 info 中获取库名
(let* ((info '((lib-name liii test lib) (line . 1))) (name (lib-name info)))
  (check name => '(liii test lib))
) ;let*

(check-report)
