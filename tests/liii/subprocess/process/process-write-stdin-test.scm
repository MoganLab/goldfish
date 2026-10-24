(import (liii check) (liii os) (liii subprocess) (scheme base))

(check-set-mode! 'report-failed)

;; process-write-stdin
;; 向子进程的标准输入写入字符串或字节序列。
;;
;; 语法
;; ----
;; (process-write-stdin proc data)
;;
;; 参数
;; ----
;; proc : process
;; 具有可写标准输入管道的原生 <process> 对象。
;;
;; data : string 或 bytevector
;; 要写入的内容字符串或字节序列。
;;
;; 返回值
;; ----
;; integer
;; 成功写入的字节数。
;;
;; 异常
;; ----
;; type-error : proc 不是 process 对象，或者 data 既不是 string 也不是 bytevector。

;; 写入字符串并返回写入字节数
(let* ((child-code (string-append "import sys\n"
                     "line = sys.stdin.readline().strip()\n"
                     "print('got:' + line, flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdin 'pipe :stdout 'pipe)
       ) ;proc
      ) ;
  (check (process-write-stdin proc "hello\n") => 6)
  (check (process-read-line proc :stream 'stdout) => "got:hello")
  (process-close-stdin proc)
  (process-close proc)
  (check (process-wait proc 2000) => 0)
) ;let*

;; 写入 bytevector 字节序列
(let* ((child-code (string-append "import sys\n"
                     "line = sys.stdin.readline().strip()\n"
                     "print('bytes:' + line, flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdin 'pipe :stdout 'pipe)
       ) ;proc
       (data (string->utf8 "scheme-bytevector\n"))
      ) ;
  (check (process-write-stdin proc data) => (bytevector-length data))
  (check (process-read-line proc :stream 'stdout) => "bytes:scheme-bytevector")
  (process-close-stdin proc)
  (process-close proc)
  (check (process-wait proc 2000) => 0)
) ;let*

;; 测试项4：标准输入分块交互与 EOF 关闭测试 (grep foo)
(when (or (os-linux?) (os-macos?))
  (let ((proc (process-spawn '("grep" "foo") :stdin 'pipe :stdout 'pipe)))
    (check (process-write-stdin proc "line 1 foo\n") => 11)
    (check (process-write-stdin proc "line 2 bar\n") => 11)
    (process-close-stdin proc)
    (check (process-read-line proc :stream 'stdout) => "line 1 foo")
    (process-close proc)
    (check (process-wait proc 2000) => 0)
  ) ;let
) ;when

;; 测试项4的跨平台兼容版本 (使用 python 进行行过滤)
(when (os-windows?)
  (let* ((filter-code (string-append "import sys\n" "for line in sys.stdin:\n"
                        "    if 'foo' in line:\n"
                        "        sys.stdout.write(line)\n"
                        "        sys.stdout.flush()\n"
                      ) ;string-append
         ) ;filter-code
         (proc
           (process-spawn `(,"python3" ,"-c" ,filter-code) :stdin 'pipe :stdout 'pipe)
         ) ;proc
        ) ;
    (check (process-write-stdin proc "line 1 foo\n") => 11)
    (check (process-write-stdin proc "line 2 bar\n") => 11)
    (process-close-stdin proc)
    (check (process-read-line proc :stream 'stdout) => "line 1 foo")
    (process-close proc)
    (check (process-wait proc 2000) => 0)
  ) ;let*
) ;when

;; 防御性类型检查
(check-catch 'type-error (process-write-stdin "not-a-proc" "data"))
(let ((proc (process-spawn '("python3" "-c" "pass") :stdin 'pipe)))
  (check-catch 'type-error (process-write-stdin proc 123))
  (check-catch 'type-error (process-write-stdin proc 'symbol))
  (process-close-stdin proc)
  (process-close proc)
  (process-wait proc 2000)
) ;let

(check-report)
