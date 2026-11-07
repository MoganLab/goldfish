;; parse-pi-args
;; 解析 Pi 命令行中的 resume 参数。
;;
;; 语法
;; ----
;; (parse-pi-args argv)
;;
;; 参数
;; ----
;; argv : list
;; 不包含可执行文件名称的字符串参数列表。
;;
;; 返回值
;; ----
;; alist
;; 解析后的参数字典。

(import (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; --resume / -r 参数解析

;; 1. 长短选项均解析为 #t
(let ((res1 (parse-pi-args '("-r")))
      (res2 (parse-pi-args '("--resume"))))
  (check (pi-args-ref res1 'resume) => #t)
  (check (pi-args-ref res2 'resume) => #t))

;; 2. 默认未指定为 #f
(check (pi-args-ref (parse-pi-args '()) 'resume) => #f)

(check-report)
