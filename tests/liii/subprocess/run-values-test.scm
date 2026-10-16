(import (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
) ;import

;; run-values
;; 执行命令并多值返回 stdout、stderr 和退出码。
;;
;; 语法
;; ----
;; (run-values command)
;; (run-values command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;;
;; keyword value ... : 可选
;; :cwd — 工作目录。
;; :env — 环境变量覆盖，格式为 '(("KEY" . "value") ...)。
;; :input — 写入子进程 stdin 的字符串。
;; :timeout — 超时时间（秒），超时后返回 -1。
;; :stdout — 'capture（默认）、'inherit、'discard、'tee 或文件路径 (string 或 path)。
;; :stderr — 'capture（默认）、'inherit、'discard、'tee、'stdout 或文件路径 (string 或 path)。
;; :stdin — 'inherit、'discard、'pipe、'null、字符串输入或文件路径 (string 或 path)。
;;
;; 返回值
;; ----
;; values : stdout-string stderr-string exit-code
;; exit-code 为 0 表示成功，-1 表示超时。

(when (or (os-linux?) (os-macos?))
  (let-values (((out err code) (run-values "echo hello" :stdout 'capture)))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  (let-values (((out err code) (run-values "false")))
    (check out => "")
    (check (zero? code) => #f)
  ) ;let-values

  ;; :env
  (let-values (((out err code) (run-values '(env) :env '(("FOO" . "bar")) :stdout 'capture)))
    (check (string-contains? out "FOO=bar") => #t)
    (check (zero? code) => #t)
  ) ;let-values

  ;; :input
  (let-values (((out err code) (run-values "cat" :input "hello world" :stdout 'capture)))
    (check out => "hello world")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :timeout
  (let-values (((out err code) (run-values "sleep 10" :timeout 1)))
    (check code => -1)
  ) ;let-values

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-values-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (let-values (((out err code) (run-values "echo hello" :stdout tmpfile)))
      (check out => "")
      (check (zero? code) => #t)
      (check (path-read-text tmpfile) => "hello\n")
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (let-values (((out err code) (run-values "echo hello" :stdout 'discard)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'stdout
  (let-values (((out err code) (run-values "echo hello" :stderr 'stdout :stdout 'capture)))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'discard
  (let-values (((out err code) (run-values "echo hello" :stderr 'discard)))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stdin from file
  (let ((tmpfile "/tmp/gf-run-values-stdin-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (call-with-output-file tmpfile (lambda (p) (display "file content\n" p)))
    (let-values (((out err code) (run-values "cat" :stdin tmpfile :stdout 'capture)))
      (check out => "file content\n")
      (check (zero? code) => #t)
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdin 'null
  (let-values (((out err code) (run-values "cat" :stdin 'null :stdout 'capture)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; list form with symbol head
  (let-values (((out err code) (run-values '(printf "%s" "hello world") :stdout 'capture)))
    (check out => "hello world")
    (check (zero? code) => #t)
  ) ;let-values
) ;when

(when (os-windows?)
  (let-values (((out err code) (run-values "python3 -c \"print('hello')\"" :stdout 'capture)))
    (check (string-trim-both out) => "hello")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :input
  (let-values (((out err code)
                (run-values "python3 -c \"import sys; print(sys.stdin.read().strip())\""
                  :input  "hello world"
                  :stdout 'capture
                ) ;run-values
               ) ;
              ) ;
    (check (string-trim-both out) => "hello world")
    (check (zero? code) => #t)
  ) ;let-values

  (let-values (((out err code) (run-values "python3 -c pass")))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  (let-values (((out err code) (run-values "python3 -c 1/0")))
    (check out => "")
    (check (zero? code) => #f)
  ) ;let-values

  ;; :env (need to preserve PATH on Windows)
  (let ((path-env (getenv "PATH"))
        (tmpfile (string-append (os-temp-dir) "/gf-run-values-env-win.txt"))
       ) ;
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (let-values (((out err code)
                  (run-values "python3 -c pass"
                    :env    `((,"FOO" . ,"bar") (,"PATH" . ,path-env))
                    :stdout tmpfile
                  ) ;run-values
                 ) ;
                ) ;
      (check (zero? code) => #t)
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :timeout
  (let-values (((out err code)
                (run-values "python3 -c \"import time; time.sleep(10)\"" :timeout 1)
               ) ;
              ) ;
    (check code => -1)
  ) ;let-values

  ;; :stdout to file
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-values-stdout-win.txt")))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (let-values (((out err code) (run-values "python3 -c print('hello')" :stdout tmpfile)))
      (check out => "")
      (check (zero? code) => #t)
      (check (path-read-text tmpfile) => "hello\n")
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (let-values (((out err code) (run-values "python3 -c print('hello')" :stdout 'discard)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'discard
  (let-values (((out err code) (run-values "python3 -c pass" :stderr 'discard)))
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stdin from file
  (let ((infile (string-append (os-temp-dir) "/gf-run-values-stdin-win.txt"))
        (outfile (string-append (os-temp-dir) "/gf-run-values-stdin-out-win.txt"))
       ) ;
    (when (file-exists? outfile)
      (delete-file outfile)
    ) ;when
    (call-with-output-file infile (lambda (p) (display "file content\n" p)))
    (let ((cmd (string-append "python3 -c \"import sys; open('"
                 (string-replace outfile "\\" "/")
                 "','w').write(sys.stdin.read())\""
               ) ;string-append
          ) ;cmd
         ) ;
      (let-values (((out err code) (run-values cmd :stdin infile)))
        (check (zero? code) => #t)
      ) ;let-values
      (check (path-read-text outfile) => "file content\n")
    ) ;let
    (when (file-exists? infile)
      (delete-file infile)
    ) ;when
    (when (file-exists? outfile)
      (delete-file outfile)
    ) ;when
  ) ;let

  ;; :stdin 'null
  (let-values (((out err code) (run-values "python3 -c pass" :stdin 'null)))
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'stdout
  (let-values (((out err code)
                (run-values "python3 -c \"import sys; sys.stdout.write('out\\n'); sys.stderr.write('err\\n')\""
                  :stderr 'stdout
                  :stdout 'capture
                ) ;run-values
               ) ;
              ) ;
    (check (string-contains? out "out") => #t)
    (check (string-contains? out "err") => #t)
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; list form with symbol head
  (run-set! 'pypass "python3")
  (let-values (((out err code) (run-values '(pypass "-c" "print('hello world')"))))
    (check (zero? code) => #t)
  ) ;let-values
) ;when

;; :env 参数类型检查回归测试
(check-catch 'type-error (run-values "true" :env '((#\a . "1"))))
(check-catch 'type-error (run-values "true" :env '((42 . "1"))))
(check-catch 'type-error (run-values "true" :env '(("FOO" . 42))))
(check-catch 'type-error (run-values "true" :env '(42)))
(check-catch 'type-error (run-values "true" :env 42))

;; 环形 env 防御回归测试
(let ((cyclic-env (list (cons "KEY" "val"))))
  (set-cdr! cyclic-env cyclic-env)
  (check-catch 'type-error (run-values "true" :env cyclic-env))
  (check-catch 'type-error (g_subprocess-run-values '("true") #f cyclic-env))
) ;let

;; 环形 command 防御回归测试
(let ((cyclic-cmd (list 'true "arg")))
  (set-cdr! (cdr cyclic-cmd) cyclic-cmd)
  (check-catch 'type-error (run-values cyclic-cmd))
  (check-catch 'type-error (g_subprocess-run-values cyclic-cmd))
) ;let

;; 验证现有 run-values 在大输出（1MB）下消除管道缓冲区满死锁
(let* ((child-code "import sys; sys.stdout.buffer.write(b'B' * (1024 * 1024)); sys.stdout.buffer.flush()"
       ) ;child-code
       (values-ret
         (let-values (((out err code) (run-values `(,"python3"
                                                    ,"-c"
                                                    ,child-code) :stdout 'capture)))
           (list (string-length out) code)
         ) ;let-values
       ) ;values-ret
      ) ;
  (check (car values-ret) => (* 1024 1024))
  (check (cadr values-ret) => 0)
) ;let*

;; 测试项1：标准错误流合并 (:stderr 'stdout) 时序测试
(let* ((order-code (string-append "import sys, time\n"
                     "sys.stdout.write('out-1\\n'); sys.stdout.flush()\n"
                     "time.sleep(0.01)\n"
                     "sys.stderr.write('err-1\\n'); sys.stderr.flush()\n"
                     "time.sleep(0.01)\n"
                     "sys.stdout.write('out-2\\n'); sys.stdout.flush()\n"
                     "time.sleep(0.01)\n"
                     "sys.stderr.write('err-2\\n'); sys.stderr.flush()\n"
                   ) ;string-append
       ) ;order-code
       (cmd `(,"python3" ,"-c" ,order-code))
      ) ;
  (let-values (((out err code) (run-values cmd :stderr 'stdout)))
    (check out
      =>
      (if (os-windows?)
        "out-1\r\nerr-1\r\nout-2\r\nerr-2\r\n"
        "out-1\nerr-1\nout-2\nerr-2\n"
      ) ;if
    ) ;check
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values
) ;let*

;; 测试项2：双路实时输出 (:stdout 'tee / :stderr 'tee) 测试
(let-values (((out err code)
              (run-values '("python3" "-c" "print('line1'); print('line2')") :stdout 'tee)
             ) ;
            ) ;
  (check out => (if (os-windows?) "line1\r\nline2\r\n" "line1\nline2\n"))
  (check err => "")
  (check (zero? code) => #t)
) ;let-values

(let-values (((out err code)
              (run-values '("python3"
                            "-c"
                            "import sys; sys.stderr.write('err1\\nerr2\\n')")
                :stderr 'tee
              ) ;run-values
             ) ;
            ) ;
  (check out => "")
  (check err => (if (os-windows?) "err1\r\nerr2\r\n" "err1\nerr2\n"))
  (check (zero? code) => #t)
) ;let-values

;; 测试项3：日志文件重定向输出测试 (path 对象支持)
(let ((out-file (path (string-append (os-temp-dir) "/gf-test-1039-out.log")))
      (err-file (path (string-append (os-temp-dir) "/gf-test-1039-err.log")))
      (in-file (path (string-append (os-temp-dir) "/gf-test-1039-in.log")))
     ) ;
  (when (file-exists? (path->string out-file))
    (delete-file (path->string out-file))
  ) ;when
  (when (file-exists? (path->string err-file))
    (delete-file (path->string err-file))
  ) ;when
  (when (file-exists? (path->string in-file))
    (delete-file (path->string in-file))
  ) ;when

  ;; 写输入文件供 stdin 重定向
  (path-write-text (path->string in-file) "redirected input content\n")

  (let-values (((out err code)
                (run-values '("python3"
                              "-c"
                              "import sys; data = sys.stdin.read(); sys.stdout.write('out:' + data); sys.stderr.write('err-msg\\n')")
                  :stdin  in-file
                  :stdout out-file
                  :stderr err-file
                ) ;run-values
               ) ;
              ) ;
    (check out => "")
    (check err => "")
    (check (zero? code) => #t)
    (check (path-read-text (path->string out-file))
      =>
      "out:redirected input content\n"
    ) ;check
    (check (path-read-text (path->string err-file)) => "err-msg\n")
  ) ;let-values

  (when (file-exists? (path->string out-file))
    (delete-file (path->string out-file))
  ) ;when
  (when (file-exists? (path->string err-file))
    (delete-file (path->string err-file))
  ) ;when
  (when (file-exists? (path->string in-file))
    (delete-file (path->string in-file))
  ) ;when
) ;let

;; 防御性类型与取值检查 (5.3 & 8.3)
(check-catch 'value-error (run-values "true" :stdout 'invalid-mode))
(check-catch 'type-error (run-values "true" :stdout 123))
(check-catch 'value-error (run-values "true" :stderr 'invalid-mode))
(check-catch 'type-error (run-values "true" :stderr 123))
(check-catch 'value-error (run-values "true" :stdin 'invalid-mode))
(check-catch 'type-error (run-values "true" :stdin 123))
(check-catch 'type-error (run-values "true" :cwd 123))

(check-report)
