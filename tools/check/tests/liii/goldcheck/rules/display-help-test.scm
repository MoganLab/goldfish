;; display-help
;; 输出 gf check 命令的命令行用法和帮助信息。
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
;;
;; 说明
;; ----
;; 打印 gf check 支持的选项、规则列表、参数说明与退出码。

(import (scheme base)
  (liii base)
  (liii check)
  (liii string)
  (liii goldcheck rules)
) ;import

(check-set-mode! 'report-failed)

(let ((out (with-output-to-string (lambda () (display-help)))))
  (check-true (string? out))
  (check-true (string-contains? out "Usage: gf check"))
  (check-true (string-contains? out "Options:"))
  (check-true (string-contains? out "Rules:"))
) ;let

(check-report)
