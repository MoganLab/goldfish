;; main
;; gf run 子命令的入口演示：打印问候语
;;
;; 语法
;; ----
;; (main)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; #t 或未指定\n副作用：向标准输出打印问候语

(import (scheme base) (liii check) (demo lib))

(check-set-mode! 'report-failed)

(check-true (procedure? main))

(check-report)
