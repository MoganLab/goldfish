(import (liii check) (liii either) (liii os) (liii string) (liii subprocess))

;; run-pipe
;; 将多个命令串联为管道（替代 shell 的 |）。
;;
;; 语法
;; ----
;; (run-pipe command ...)
;; (run-pipe command ... keyword value ...)
;;
;; 参数
;; ----
;; command ... : 多个命令。
;;
;; keyword value ... : 可选
;; :cwd、:env、:timeout — 共享选项。
;;
;; 返回值
;; ----
;; Either
;; 成功时返回 Right（内含管道最后一级的 stdout 字符串），
;; 失败时返回 Left（内含 <process-error> 记录）。

(when (or (os-linux?) (os-macos?))
  (check (either-right? (run-pipe "echo hello world" '(grep "hello"))) => #t)
  (check (to-right (run-pipe "echo hello world" '(grep "hello")))
    =>
    "hello world\n"
  ) ;check
  (check (either-right? (run-pipe '(printf "a\nb\nc") '(grep "a") '(wc "-l")))
    =>
    #t
  ) ;check
  (check
    (string-trim-both
      (to-right (run-pipe '(printf "a\nb\nc") '(grep "a") '(wc "-l")))
    ) ;string-trim-both
    =>
    "1"
  ) ;check
  (check (either-right? (run-pipe "echo hello")) => #t)
  (check (to-right (run-pipe "echo hello")) => "hello\n")

  (let ((res (run-pipe "echo hello" '(false))))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => '(false))
    ) ;let
  ) ;let
) ;when

(when (os-windows?)
  (check (either-right? (run-pipe "python3 -c \"print('hello world')\""
                          "python3 -c \"import sys; print(sys.stdin.read().strip())\""
                        ) ;run-pipe
         ) ;either-right?
    =>
    #t
  ) ;check
  (check (string-trim-both (to-right (run-pipe "python3 -c \"print('hello world')\""
                                       "python3 -c \"import sys; print(sys.stdin.read().strip())\""
                                     ) ;run-pipe
                           ) ;to-right
         ) ;string-trim-both
    =>
    "hello world"
  ) ;check
  (check (either-right? (run-pipe "python3 -c pass")) => #t)
  (let ((res (run-pipe "python3 -c \"print('hello')\"" "python3 -c 1/0")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "python3 -c 1/0")
    ) ;let
  ) ;let
) ;when

(check-report)
