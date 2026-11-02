;; display-help
;; 输出 gf check 命令的命令行用法和帮助信息（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (display-help)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; unspecified
;; 将帮助文本打印到当前输出端口。

(import (scheme base) (liii base) (liii check) (liii string) (liii goldcheck))

(check-set-mode! 'report-failed)

;; 1. 打印帮助信息包含关键内容
(let ((out (with-output-to-string (lambda () (display-help)))))
  (check-true (string? out))
  (check-true (string-contains? out "Usage: gf check"))
) ;let

(check-report)
