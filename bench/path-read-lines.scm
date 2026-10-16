;; path-read-lines 性能基准测试
;; 测试 (liii path) 中 path-read-lines 的性能，为 C++ 实现提供基准数据

(import (liii timeit) (liii path) (liii os) (scheme base))

(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

;; 生成包含 n 行的文本内容
(define (make-lines n)
  (let loop
    ((i 0) (parts '()))
    (if (= i n)
      (apply string-append (reverse parts))
      (loop (+ i 1) (cons (string-append "line-" (number->string i) "\n") parts))
    ) ;if
  ) ;loop
) ;define

(define small-file (path-join (path-temp-dir) "bench-read-lines-small.txt"))
(define large-file (path-join (path-temp-dir) "bench-read-lines-large.txt"))

(define (setup)
  (path-write-text small-file (make-lines 100))
  (path-write-text large-file (make-lines 100000))
) ;define

(define (teardown)
  (when (path-exists? small-file)
    (delete-file (path->string small-file))
  ) ;when
  (when (path-exists? large-file)
    (delete-file (path->string large-file))
  ) ;when
) ;define

(define (run-benchmarks)
  (display "=== path-read-lines 性能测试 ===\n\n")

  (bench "小文件(100行) 默认          "
    (lambda () (path-read-lines small-file))
    1000
  ) ;bench

  (bench "小文件(100行) keepends     "
    (lambda () (path-read-lines small-file #t))
    1000
  ) ;bench

  (bench "大文件(10万行) 默认        "
    (lambda () (path-read-lines large-file))
    10
  ) ;bench

  (bench "大文件(10万行) keepends    "
    (lambda () (path-read-lines large-file #t))
    10
  ) ;bench

  (bench "大文件(10万行) offset=50000"
    (lambda () (path-read-lines large-file #f 50000))
    10
  ) ;bench

  (bench "大文件(10万行) limit=100   "
    (lambda () (path-read-lines large-file #f 0 100))
    1000
  ) ;bench

  (bench "大文件 offset+limit 切片   "
    (lambda () (path-read-lines large-file #f 50000 1000))
    100
  ) ;bench
) ;define

(setup)
(run-benchmarks)
(teardown)
