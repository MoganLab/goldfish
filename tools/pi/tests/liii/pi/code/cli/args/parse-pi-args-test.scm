;; parse-pi-args
;; 解析 gf pi 命令行参数为 <pi-cli-args> 记录（含默认字段）。
;;
;; 语法
;; ----
;; (parse-pi-args args) -> <pi-cli-args>
;;
;; 参数
;; ----
;; args : list
;; 命令行参数字符串列表。
;;
;; 返回值
;; ------
;; <pi-cli-args>
;; 解析结果记录；未出现的字段取默认值。

(import (scheme base) (liii check) (liii pi code cli args))

(check-set-mode! 'report-failed)

;; 空参数：默认值与 record 谓词
(let ((res (parse-pi-args '())))
  (check-true (pi-cli-args? res))
  (check (pi-args-ref res 'provider) => #f)
  (check (pi-args-ref res 'print) => #f)
  (check (pi-args-ref res 'skill-paths) => '())
) ;let

;; 基本标志与带值参数
(let ((res (parse-pi-args '("-p" "--model" "gemini-3.8-flash" "--no-skills"))))
  (check (pi-args-ref res 'print) => #t)
  (check (pi-args-ref res 'model) => "gemini-3.8-flash")
  (check (pi-args-ref res 'no-skills) => #t)
) ;let

;; 可重复参数累加
(let ((res (parse-pi-args '("--append-system-prompt" "one"
                            "--append-system-prompt" "two"))
      ) ;res
     ) ;
  (check (pi-args-ref res 'append-system-prompt) => '("one" "two"))
) ;let

;; 位置消息与未知标志
(let ((res (parse-pi-args '("hello" "--unknown-flag"))))
  (check (pi-args-ref res 'messages) => '("hello"))
  (check-true (pair? (member "--unknown-flag" (pi-args-ref res 'unknown-flags '())))
  ) ;check-true
) ;let

(check-report)
