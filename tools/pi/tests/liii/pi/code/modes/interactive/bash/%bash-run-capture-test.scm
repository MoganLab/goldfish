;; %bash-run-capture
;; ! 命令无中止探测的同步捕获路径（经 run-either）。
;;
;; 语法
;; ----
;; (%bash-run-capture command timeout)
;;
;; 参数
;; ----
;; command : string
;; 要执行的 bash 命令。
;; timeout : real
;; 超时秒数。
;;
;; 返回值
;; ----
;; (values 清理后的合并输出 退出码)：-1 超时（run-either 约定）。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常命令：返回真实退出码，输出已清理（剥 ANSI、删 \r）
(let-values (((text code) (%bash-run-capture "printf 'red\\033[0m\\r\\n'" 30))
           ) ;
  (check code => 0)
  (check-true (string-contains? text "red"))
) ;let-values

;; 2. 非零退出码：exit 42 原样返回
(let-values (((text code) (%bash-run-capture "exit 42" 30))
           ) ;
  (check code => 42)
) ;let-values

;; 3. 超时：sleep 30 以 1s 超时强杀，返回 -1
(let-values (((text code) (%bash-run-capture "sleep 30" 1))
           ) ;
  (check code => -1)
) ;let-values

(check-report)
