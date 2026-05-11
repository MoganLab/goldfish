;; utf8-string-set! 性能基准测试
;; 收集优化前的性能数据，用于对比优化后的效果

(import (liii timeit) (liii unicode) (scheme base))

(define (bench name stmt-call number)
  (let ((elapsed (timeit stmt-call '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

(define (make-ascii-string n)
  (utf8-make-string n #\a)
) ;define

(define (make-chinese-string n)
  (utf8-make-string n #\中)
) ;define

(define (run-benchmarks)
  (display "=== utf8-string-set! 性能测试 ===\n\n")

  (display "--- 同长度替换 (ASCII → ASCII) ---\n")
  (let ((s10 (make-ascii-string 10)))
    (bench "长度 10  开头" (lambda () (utf8-string-set! s10 0 #\X)) 100000)
    (bench "长度 10  中间" (lambda () (utf8-string-set! s10 5 #\X)) 100000)
    (bench "长度 10  末尾" (lambda () (utf8-string-set! s10 9 #\X)) 100000)
  ) ;let
  (let ((s100 (make-ascii-string 100)))
    (bench "长度 100 中间" (lambda () (utf8-string-set! s100 50 #\X)) 10000)
  ) ;let
  (let ((s1000 (make-ascii-string 1000)))
    (bench "长度 1000 中间" (lambda () (utf8-string-set! s1000 500 #\X)) 1000)
  ) ;let
  (let ((s10000 (make-ascii-string 10000)))
    (bench "长度 10000 中间" (lambda () (utf8-string-set! s10000 5000 #\X)) 100)
  ) ;let
  (newline)

  (display "--- 不同长度替换 (ASCII → 中文, 1字节→3字节) ---\n")
  (let ((s10 (make-ascii-string 10)))
    (bench "长度 10  开头" (lambda () (utf8-string-set! s10 0 #\中)) 100000)
    (bench "长度 10  中间" (lambda () (utf8-string-set! s10 5 #\中)) 100000)
    (bench "长度 10  末尾" (lambda () (utf8-string-set! s10 9 #\中)) 100000)
  ) ;let
  (let ((s100 (make-ascii-string 100)))
    (bench "长度 100 中间" (lambda () (utf8-string-set! s100 50 #\中)) 10000)
  ) ;let
  (let ((s1000 (make-ascii-string 1000)))
    (bench "长度 1000 中间" (lambda () (utf8-string-set! s1000 500 #\中)) 1000)
  ) ;let
  (let ((s10000 (make-ascii-string 10000)))
    (bench "长度 10000 中间"
      (lambda () (utf8-string-set! s10000 5000 #\中))
      100
    ) ;bench
  ) ;let
  (newline)

  (display "--- 不同长度替换 (中文 → ASCII, 3字节→1字节) ---\n")
  (let ((s10 (make-chinese-string 10)))
    (bench "长度 10  开头" (lambda () (utf8-string-set! s10 0 #\X)) 100000)
    (bench "长度 10  中间" (lambda () (utf8-string-set! s10 5 #\X)) 100000)
  ) ;let
  (let ((s100 (make-chinese-string 100)))
    (bench "长度 100 中间" (lambda () (utf8-string-set! s100 50 #\X)) 10000)
  ) ;let
  (let ((s1000 (make-chinese-string 1000)))
    (bench "长度 1000 中间" (lambda () (utf8-string-set! s1000 500 #\X)) 1000)
  ) ;let
  (newline)
) ;define

(run-benchmarks)
