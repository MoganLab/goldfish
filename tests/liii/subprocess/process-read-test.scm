(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-read
;; 从子进程的标准输出或标准错误读取数据块。
;;
;; 语法
;; ----
;; (process-read proc)
;; (process-read proc keyword value ...)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; keyword value ... : 可选
;; :max-bytes — 最多读取的字节数正整数（默认为 65536）。
;; :stream — 读取的数据流，'stdout（默认）或 'stderr。
;;
;; 返回值
;; ----
;; string 或 eof-object
;; 读取到的字符串；当流关闭且无更多数据时返回 EOF 对象。

(let* ((child-code (string-append "import sys\n"
                     "sys.stdout.write('chunk-data')\n"
                     "sys.stdout.flush()\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe)
       ) ;proc
      ) ;
  (let ((data (process-read proc :max-bytes 1024 :stream 'stdout)))
    (check data => "chunk-data")
  ) ;let
  (let ((eof (process-read proc :max-bytes 1024 :stream 'stdout)))
    (check (eof-object? eof) => #t)
  ) ;let
  (process-close proc)
  (process-wait proc 2000)
) ;let*

;; 大数据量流式管道读取 (process-read)
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

;; 错误处理
(check-catch 'type-error (process-read "invalid"))
(let ((proc (process-spawn '("python3" "-c" "pass") :stdout 'pipe)))
  (check-catch 'type-error (process-read proc :max-bytes -1))
  (check-catch 'type-error (process-read proc :max-bytes "bad"))
  (check-catch 'value-error (process-read proc :stream 'invalid-stream))
  (process-close proc)
  (process-wait proc 2000)
) ;let

(check-report)
