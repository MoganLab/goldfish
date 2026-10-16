(import (liii check)
  (liii either)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
) ;import

;; run-either
;; 执行命令并返回标准输出或错误信息。
;;
;; 语法
;; ----
;; (run-either command)
;; (run-either command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;;
;; keyword value ... : 可选
;; :cwd — 工作目录。
;; :env — 环境变量覆盖。
;; :input — 写入子进程 stdin 的字符串。
;; :timeout — 超时时间（秒）。
;; :stdout — 'capture、'discard 或文件路径。
;; :stderr — 'capture、'stdout、'discard 或文件路径。
;; :stdin — 文件路径或 'null。
;;
;; 返回值
;; ----
;; Either
;; 成功时返回 Right（内含标准输出字符串），
;; 失败时返回 Left（内含 <process-error> 记录类型）。

(when (or (os-linux?) (os-macos?))
  ;; basic success
  (check (either-right? (run-either "echo hello" :stdout 'capture)) => #t)
  (check (to-right (run-either "echo hello" :stdout 'capture)) => "hello\n")
  (check (either-right? (run-either '(echo "hello") :stdout 'capture)) => #t)
  (check (to-right (run-either '(echo "hello") :stdout 'capture)) => "hello\n")

  ;; basic failure
  (check (either-left? (run-either "false")) => #t)
  (let ((err (to-left (run-either "false"))))
    (check (process-error? err) => #t)
    (check (process-error-code err) => 1)
    (check (string? (process-error-stderr err)) => #t)
    (check (string? (process-error-stdout err)) => #t)
    (check (process-error-command err) => "false")
  ) ;let

  ;; failure with stderr
  (check (either-left? (run-either "ls /nonexistent" :stderr 'capture)) => #t)
  (let ((err (to-left (run-either "ls /nonexistent" :stderr 'capture))))
    (check (process-error? err) => #t)
    (check (number? (process-error-code err)) => #t)
    (check (string? (process-error-stderr err)) => #t)
    (check (> (string-length (process-error-stderr err)) 0) => #t)
    (check (string? (process-error-message err)) => #t)
    (check (> (string-length (process-error-message err)) 0) => #t)
  ) ;let

  ;; :cwd
  (let ((res (run-either "pwd" :cwd "/tmp" :stdout 'capture)))
    (check (either-right? res) => #t)
    (check (or (string=? (to-right res) "/tmp\n")
             (string=? (to-right res) "/private/tmp\n")
           ) ;or
      =>
      #t
    ) ;check
  ) ;let

  ;; :env
  (let ((result (run-either '(env) :env '(("FOO" . "bar")) :stdout 'capture)))
    (check (either-right? result) => #t)
    (check (string-contains? (to-right result) "FOO=bar") => #t)
  ) ;let

  ;; :input
  (check (either-right? (run-either "cat" :input "hello world" :stdout 'capture))
    =>
    #t
  ) ;check
  (check (to-right (run-either "cat" :input "hello world" :stdout 'capture))
    =>
    "hello world"
  ) ;check

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-either-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-either "echo hello" :stdout tmpfile)) => #t)
    (check (to-right (run-either "echo hello" :stdout tmpfile)) => "")
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (check (either-right? (run-either "echo hello" :stdout 'discard)) => #t)
  (check (to-right (run-either "echo hello" :stdout 'discard)) => "")

  ;; :stderr 'stdout
  (check (either-right? (run-either "echo hello" :stderr 'stdout :stdout 'capture))
    =>
    #t
  ) ;check
  (check (to-right (run-either "echo hello" :stderr 'stdout :stdout 'capture))
    =>
    "hello\n"
  ) ;check

  ;; :stderr 'discard
  (check (either-right? (run-either "echo hello" :stderr 'discard)) => #t)
  (check (to-right (run-either "echo hello" :stderr 'discard)) => "hello\n")

  ;; list form with symbol head
  (check (either-right? (run-either '(printf "%s" "hello world") :stdout 'capture))
    =>
    #t
  ) ;check
  (check (to-right (run-either '(printf "%s" "hello world") :stdout 'capture))
    =>
    "hello world"
  ) ;check
) ;when

(when (os-windows?)
  ;; basic success
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-out-win.txt")))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-either "python3 -c pass" :stdout tmpfile :stderr 'discard))
      =>
      #t
    ) ;check
    (check (to-right (run-either "python3 -c pass" :stdout tmpfile :stderr 'discard))
      =>
      ""
    ) ;check
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; basic failure
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-out2-win.txt")))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-left? (run-either "python3 -c 1/0" :stdout tmpfile :stderr 'discard))
      =>
      #t
    ) ;check
    (let ((result (to-left (run-either "python3 -c 1/0" :stdout tmpfile :stderr 'discard))
          ) ;result
         ) ;
      (check (process-error? result) => #t)
      (check (number? (process-error-code result)) => #t)
      (check (string? (process-error-stderr result)) => #t)
    ) ;let
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :cwd
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-cwd-win.txt")))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-either "python3 -c pass"
                            :cwd    (os-temp-dir)
                            :stdout tmpfile
                            :stderr 'discard
                          ) ;run-either
           ) ;either-right?
      =>
      #t
    ) ;check
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :env
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-env-win.txt"))
        (path-env (getenv "PATH"))
       ) ;
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check
      (either-right?
        (run-either "python3 -c pass"
          :env    `(("FOO" . "bar") ("PATH" . ,path-env))
          :stdout tmpfile
          :stderr 'discard
        ) ;run-either
      ) ;either-right?
      =>
      #t
    ) ;check
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout to file
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-file-win.txt")))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-either "python3 -c print('hello')" :stdout tmpfile :stderr 'discard)
           ) ;either-right?
      =>
      #t
    ) ;check
    (check (to-right (run-either "python3 -c print('hello')" :stdout tmpfile :stderr 'discard)
           ) ;to-right
      =>
      ""
    ) ;check
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (check (either-right? (run-either "python3 -c pass" :stdout 'discard :stderr 'discard))
    =>
    #t
  ) ;check
  (check (to-right (run-either "python3 -c pass" :stdout 'discard :stderr 'discard))
    =>
    ""
  ) ;check

  ;; list form with symbol head
  (run-set! 'pypass "python3")
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-list-win.txt")))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-either '(pypass "-c" "print('hello world')")
                            :stdout tmpfile
                            :stderr 'discard
                          ) ;run-either
           ) ;either-right?
      =>
      #t
    ) ;check
    (check (to-right (run-either '(pypass "-c" "print('hello world')")
                       :stdout tmpfile
                       :stderr 'discard
                     ) ;run-either
           ) ;to-right
      =>
      ""
    ) ;check
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let
) ;when

;; 参数防御与错误类型回归测试
(check-catch 'type-error (run-either "true" :env '((#\a . "1"))))
(check-catch 'type-error (run-either "true" :env '((42 . "1"))))
(check-catch 'type-error (run-either "true" :env '(("FOO" . 42))))
(check-catch 'type-error (run-either "true" :env '(42)))
(check-catch 'type-error (run-either "true" :env 42))

;; 环形 env 防御回归测试
(let ((cyclic-env (list (cons "KEY" "val"))))
  (set-cdr! cyclic-env cyclic-env)
  (check-catch 'type-error (run-either "true" :env cyclic-env))
) ;let

;; 环形 command 防御回归测试
(let ((cyclic-cmd (list 'true "arg")))
  (set-cdr! (cdr cyclic-cmd) cyclic-cmd)
  (check-catch 'type-error (run-either cyclic-cmd))
) ;let

(check-catch 'value-error (run-either "true" :stdout 'invalid-mode))
(check-catch 'type-error (run-either "true" :stdout 123))
(check-catch 'value-error (run-either "true" :stderr 'invalid-mode))
(check-catch 'type-error (run-either "true" :stderr 123))
(check-catch 'value-error (run-either "true" :stdin 'invalid-mode))
(check-catch 'type-error (run-either "true" :stdin 123))
(check-catch 'type-error (run-either "true" :cwd 123))

(check-report)
