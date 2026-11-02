;; lib-imports
;; 从库解析信息 alist 中提取导入的库列表。
;;
;; 语法
;; ----
;; (lib-imports info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; list of list
;; 导入库符号列表（如 '((scheme base) (liii string))）。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 获取导入库列表
(let* ((info '((imports (scheme base) (liii base)))) (imps (lib-imports info)))
  (check imps => '((scheme base) (liii base)))
) ;let*

(check-report)
