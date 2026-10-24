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
  (check-catch 'value-error (process-read proc :max-bytes -1))
  (check-catch 'type-error (process-read proc :max-bytes "bad"))
  (check-catch 'value-error (process-read proc :stream 'invalid-stream))
  (check-catch 'type-error (process-read proc :stream 123))
  (process-close proc)
  (process-wait proc 2000)
) ;let

;; 进程退出后读取数据不丢失：子进程写入并退出后，父进程先 wait 再 read，
;; 必须读到全部输出。修复前 Windows 分支存在竞态：Peek 无数据时若恰好检测
;; 到进程已退出则直接置 EOF，退出瞬间 flush 的数据被丢弃（CI 偶发
;; pi-json-e2e 段 anthropic 空输出的根因）
(let* ((child-code (string-append "import sys\n"
                   "sys.stdout.write('flush-at-exit')\n"
                 ) ;string-append
       ) ;child-code
      ) ;let*
  (let loop ((i 0))
    (if (< i 20)
      (let* ((proc (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe))
             (code (process-wait proc 5000))
             (data (process-read proc :max-bytes 65536 :stream 'stdout))
            ) ;let*
        (check code => 0)
        (check data => "flush-at-exit")
        (process-close proc)
        (loop (+ i 1))
      ) ;let*
    ) ;if
  ) ;let
) ;let*

;; 读写交错场景：父进程持续轮询读，子进程短暂存活后打印并退出，
;; 轮询窗口内数据不得丢失
(let* ((child-code (string-append "import sys\n"
                   "sys.stdout.write('quick-exit-data')\n"
                 ) ;string-append
       ) ;child-code
      ) ;let*
  (let loop ((i 0))
    (if (< i 20)
      (let* ((proc (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe))
             (acc
               (let inner ((acc ""))
                 (let ((chunk (process-read proc :max-bytes 65536 :stream 'stdout)))
                   (if (eof-object? chunk)
                     acc
                     (inner (string-append acc chunk))
                   ) ;if
                 ) ;let
               ) ;let
             ) ;acc
             (code (process-wait proc 5000))
            ) ;let*
        (check code => 0)
        (check acc => "quick-exit-data")
        (process-close proc)
        (loop (+ i 1))
      ) ;let*
    ) ;if
  ) ;let
) ;let*

(check-report)
