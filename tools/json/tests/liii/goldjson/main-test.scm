;; main
;; gf json 子命令入口：从命令行或显式参数提取参数并执行主流程。
;;
;; 语法
;; ----
;; (main)
;; (main arg ...)
;;
;; 参数
;; ----
;; arg ... : string?
;; 可选的显式参数列表。无参数时从 (command-line) 提取，
;; 定位 "json" 标记后的部分作为 gf json 的参数。
;;
;; 返回值
;; -----
;; integer
;; 进程退出码：0 表示成功，1 表示参数或解析错误。
;;
;; 说明
;; ----
;; tools/json/main.scm 以 (exit (main)) 方式调用；
;; 显式传参形式供单元测试使用，行为与命令行一致。

(set! *load-path* (cons "tools/json" *load-path*))

(import (scheme base) (scheme write) (liii check) (liii goldjson))

(check-set-mode! 'report-failed)

;; 1. 显式 --help 返回 0（输出重定向到字符串端口）
(let ((out (open-output-string)))
  (let ((orig (current-output-port)))
    (set-current-output-port out)
    (check (main "--help") => 0)
    (set-current-output-port orig)
  ) ;let
) ;let

;; 2. 显式非法选项返回 1（错误提示重定向到字符串端口）
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (main "--no-such-flag") => 1)
  (set-current-error-port orig-err)
) ;let

;; 3. 显式参数执行转换（重定向 stdin / stdout）
(let* ((in (open-input-string "{\"b\":2,\"a\":1}"))
       (out (open-output-string))
       (orig-in (current-input-port))
       (orig-out (current-output-port))
      ) ;
  (set-current-input-port in)
  (set-current-output-port out)
  (let ((ret (main "--compact" "--sort-keys")))
    (set-current-input-port orig-in)
    (set-current-output-port orig-out)
    (check ret => 0)
    (check (get-output-string out) => "{\"a\":1,\"b\":2}\n")
  ) ;let
) ;let*

(check-report)
