;; run-goldpi
;; 运行 Goldfish Scheme Pi CLI 入口，run-pi 的别名。
;;
;; 语法
;; ----
;; (run-goldpi raw-args)
;;
;; 参数
;; ----
;; raw-args : list?
;; 原始命令行字符串列表，可包含可执行文件路径及 goldpi/pi 子命令前缀。
;;
;; 返回值
;; ----
;; integer?
;; 进程风格退出码：成功为 0，参数、安装或运行错误为 1。

;; 先导入 (liii pi code cli)：若 (liii check) 先于它导入，编译器在宏展开期
;; 会拿到已回收的常量（报 "lambda parameter is a constant: <free cell!>"）。
(import (liii pi code cli) (liii check) (liii string))

(check-set-mode! 'report-failed)

(define (check-help-command raw-args)
  (let ((status #f))
    (let ((output
            (with-output-to-string (lambda () (set! status (run-goldpi raw-args))))
          ) ;output
         ) ;
      (check status => 0)
      (check-true (string-contains? output "Usage:"))
    ) ;let
  ) ;let
) ;define

(check-help-command '("gf" "goldpi" "--help"))
(check-help-command '("/usr/bin/gf" "--help"))


(check-report)
