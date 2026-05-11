;; utf8-make-string 性能基准测试
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

(define (run-benchmarks)
  (display "=== utf8-make-string 性能测试 ===\n\n")

  (display "--- ASCII 字符 (\\#a, 1 字节) ---\n")
  (bench "长度 1      " (lambda () (utf8-make-string 1 #\a)) 100000)
  (bench "长度 10     " (lambda () (utf8-make-string 10 #\a)) 100000)
  (bench "长度 100    " (lambda () (utf8-make-string 100 #\a)) 10000)
  (bench "长度 1000   " (lambda () (utf8-make-string 1000 #\a)) 1000)
  (bench "长度 10000  " (lambda () (utf8-make-string 10000 #\a)) 100)
  (newline)

  (display "--- 中文字符 (\\#中, 3 字节) ---\n")
  (bench "长度 1      " (lambda () (utf8-make-string 1 #\中)) 100000)
  (bench "长度 10     " (lambda () (utf8-make-string 10 #\中)) 100000)
  (bench "长度 100    " (lambda () (utf8-make-string 100 #\中)) 10000)
  (bench "长度 1000   " (lambda () (utf8-make-string 1000 #\中)) 1000)
  (bench "长度 10000  " (lambda () (utf8-make-string 10000 #\中)) 100)
  (newline)

  (display "--- Emoji 字符 (\\#🚀, 4 字节) ---\n")
  (bench "长度 1      " (lambda () (utf8-make-string 1 #\🚀)) 100000)
  (bench "长度 10     " (lambda () (utf8-make-string 10 #\🚀)) 100000)
  (bench "长度 100    " (lambda () (utf8-make-string 100 #\🚀)) 10000)
  (bench "长度 1000   " (lambda () (utf8-make-string 1000 #\🚀)) 1000)
  (bench "长度 10000  " (lambda () (utf8-make-string 10000 #\🚀)) 100)
  (newline)
) ;define

(run-benchmarks)
