(import (liii check) (liii subprocess) (liii string))

;; 测试项1：10MB 大数据量无死锁流式管道读取 (process-read)
(let* ((data-size (* 10 1024 1024))
       (chunk-size (* 64 1024))
       (chunks-count (quotient data-size chunk-size))
       (child-code (string-append "import sys\n"
                     "chunk = b'A' * "
                     (number->string chunk-size)
                     "\n"
                     "for _ in range("
                     (number->string chunks-count)
                     "):\n"
                     "    sys.stdout.buffer.write(chunk)\n"
                     "sys.stdout.buffer.flush()\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe)
       ) ;proc
      ) ;
  (let loop
    ((total-bytes 0))
    (let ((chunk (process-read proc :max-bytes 65536 :stream 'stdout)))
      (if (eof-object? chunk)
        (begin
          (process-close proc)
          (let ((code (process-wait proc 10000)))
            (check code => 0)
            (check total-bytes => data-size)
          ) ;let
        ) ;begin
        (loop (+ total-bytes (string-length chunk)))
      ) ;if
    ) ;let
  ) ;let
) ;let*

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

;; 测试项2：实时增量逐行消费测试 (process-read-line)
(let* ((child-code (string-append "import sys, time\n" "for i in range(3):\n"
                     "    print(f'line_{i}', flush=True)\n"
                     "    time.sleep(0.05)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe)
       ) ;proc
      ) ;
  (check (process-read-line proc :stream 'stdout) => "line_0")
  (check (process-read-line proc :stream 'stdout) => "line_1")
  (check (process-read-line proc :stream 'stdout) => "line_2")
  (check (eof-object? (process-read-line proc :stream 'stdout)) => #t)
  (process-close proc)
  (check (process-wait proc 2000) => 0)
) ;let*

;; 测试项3：标准输入双向流式交互 (process-write / process-close-stdin)
(let* ((child-code (string-append "import sys\n" "for line in sys.stdin:\n"
                     "    line = line.strip()\n" "    if line == 'QUIT':\n"
                     "        break\n" "    print('echo:' + line, flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdin 'pipe :stdout 'pipe)
       ) ;proc
      ) ;
  (process-write proc "hello\n")
  (check (process-read-line proc :stream 'stdout) => "echo:hello")
  (process-write proc "world\n")
  (check (process-read-line proc :stream 'stdout) => "echo:world")
  (process-write proc "QUIT\n")
  (process-close-stdin proc)
  (check (eof-object? (process-read-line proc :stream 'stdout)) => #t)
  (process-close proc)
  (check (process-wait proc 2000) => 0)
) ;let*

;; 测试项4：高层级一站式流式运行器 (run-streaming)
(let* ((lines '())
       (child-code (string-append "import sys\n" "print('alpha', flush=True)\n"
                     "print('beta', flush=True)\n"
                     "print('gamma', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (exit-code
         (run-streaming `(,"python3" ,"-c" ,child-code)
           :on-stdout (lambda (line) (set! lines (cons line lines)))
         ) ;run-streaming
       ) ;exit-code
      ) ;
  (check exit-code => 0)
  (check (reverse lines) => '("alpha" "beta" "gamma"))
) ;let*

;; 测试项5：类型错误与参数校验
(check-catch 'type-error (process-read "not-proc"))
(check-catch 'type-error (process-read-line 42))
(check-catch 'type-error (process-write 42 "msg"))
(check-catch 'type-error (process-close-stdin "proc"))
(check-catch 'value-error (process-spawn '("true") :stdout 'invalid-sym))
(check-catch 'value-error (process-spawn '("true") :stderr 'invalid-sym))
(check-catch 'value-error (process-spawn '("true") :stdin 'invalid-sym))

(check-report)
