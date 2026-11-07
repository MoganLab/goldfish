;; run-simplify-flow
;; 执行代码简化流程，支持只读建议与应用修改两种模式。
;;
;; 语法
;; ----
;; (run-simplify-flow parsed cfg mode)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; cfg : list?
;; 工具配置 alist。
;;
;; mode : symbol?
;; 运行模式，如 'default 或 'json。
;;
;; 返回值
;; ----
;; integer?
;; 退出状态码，0 表示正常完成。

(import (liii base)
        (liii check)
        (liii pi simplify modes)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证过程对象与参数数量
(check-true (procedure? run-simplify-flow))
(check (arity run-simplify-flow) => '(3 . 3))

(check-report)
