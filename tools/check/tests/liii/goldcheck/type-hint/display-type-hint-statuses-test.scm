;; display-type-hint-statuses
;; 输出 hint 库的覆盖率与测试结果状态摘要到当前输出端口。
;;
;; 语法
;; ----
;; (display-type-hint-statuses violations)
;;
;; 参数
;; ----
;; violations : list  检查产生的违规列表
;;
;; 返回值
;; ------
;; unspecified
;; 将状态行输出到当前输出端口。

(import (scheme base) (scheme write) (liii check) (liii goldcheck type-hint))

(check-set-mode! 'report-failed)

;; 1. 验证过程调用不抛异常
(let ((out (with-output-to-string (lambda () (display-type-hint-statuses '())))))
  (check-true (string? out))
) ;let

(check-report)
