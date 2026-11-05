;; display-help
;; 输出 gf fix 命令的用法与帮助信息。
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

(import (scheme base) (liii base) (liii check) (liii string) (liii goldfix cli))

(check-set-mode! 'report-failed)

(let ((out (with-output-to-string (lambda () (display-help)))))
  (check-true (string? out))
  (check-true (string-contains? out "Usage: gf fix"))
  (check-true (string-contains? out "Options:"))
  (check-true (string-contains? out "Examples:"))
) ;let

(check-report)
