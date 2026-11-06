;; parse-pi-args
;; 解析 gf pi 命令行参数为 alist（含默认键）。
;;
;; 语法
;; ----
;; (parse-pi-args args) -> alist
;;
;; 参数
;; ----
;; args : list
;; 命令行参数字符串列表。
;;
;; 返回值
;; ------
;; alist
;; 解析结果；未出现的键取默认值。

(import (scheme base)
  (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; 空参数：默认值
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'provider) => #f)
  (check (pi-args-ref res 'print) => #f)
  (check (pi-args-ref res 'skill-paths) => '()))

;; 基本标志与带值参数
(let ((res (parse-pi-args '("-p" "--model" "gemini-3.8-flash" "--no-skills"))))
  (check (pi-args-ref res 'print) => #t)
  (check (pi-args-ref res 'model) => "gemini-3.8-flash")
  (check (pi-args-ref res 'no-skills) => #t))

;; 可重复参数累加
(let ((res (parse-pi-args '("--append-system-prompt" "one" "--append-system-prompt" "two"))))
  (check (pi-args-ref res 'append-system-prompt) => '("one" "two")))

;; 位置消息与未知标志
(let ((res (parse-pi-args '("hello" "--unknown-flag"))))
  (check (pi-args-ref res 'messages) => '("hello"))
  (check-true (pair? (member "--unknown-flag" (pi-args-ref res 'unknown-flags '())))))

(check-report)
