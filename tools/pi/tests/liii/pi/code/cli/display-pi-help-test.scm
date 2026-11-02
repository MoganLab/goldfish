;; display-pi-help
;; 输出 Pi 命令行工具的使用帮助信息。
;;
;; 语法
;; ----
;; (display-pi-help)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;; 将帮助文本输出到当前输出端口。

;; 先导入 (liii pi code cli) 避免宏展开期常量回收
(import (liii pi code cli) (liii check) (liii string))

(check-set-mode! 'report-failed)

(let ((out (with-output-to-string (lambda () (display-pi-help)))))
  (check-true (string-contains? out "Usage:"))
  (check-true (string-contains? out "--help"))
  (check-true (string-contains? out "--list-models"))
) ;let


(check-report)
