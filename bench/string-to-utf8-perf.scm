;; string->utf8 性能基准测试
;; 收集优化前的性能数据，用于对比后续 native C++ 实现的效果
;; 字符串预构造，bench 闭包只调用 string->utf8，避免构造开销污染测量

(import (liii timeit) (scheme base) (liii unicode))

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

;; 构造指定字符重复 n 次的字符串
;; ASCII 用 make-string；多字节字符用 utf8-make-string（make-string 对多字节字符会生成非法 UTF-8）

(define (repeat-char ch n)
  (if (< (char->integer ch) 128) (make-string n ch) (utf8-make-string n ch))
) ;define

;; 预构造所有测试字符串

(define s-a-1 (repeat-char #\a 1))

(define s-a-10 (repeat-char #\a 10))

(define s-a-100 (repeat-char #\a 100))

(define s-a-1000 (repeat-char #\a 1000))

(define s-a-10000 (repeat-char #\a 10000))

(define s-la-1 (repeat-char #\ä 1))

(define s-la-10 (repeat-char #\ä 10))

(define s-la-100 (repeat-char #\ä 100))

(define s-la-1000 (repeat-char #\ä 1000))

(define s-la-10000 (repeat-char #\ä 10000))

(define s-zh-1 (repeat-char #\中 1))

(define s-zh-10 (repeat-char #\中 10))

(define s-zh-100 (repeat-char #\中 100))

(define s-zh-1000 (repeat-char #\中 1000))

(define s-zh-10000 (repeat-char #\中 10000))

(define s-emoji-1 (repeat-char #\🚀 1))

(define s-emoji-10 (repeat-char #\🚀 10))

(define s-emoji-100 (repeat-char #\🚀 100))

(define s-emoji-1000 (repeat-char #\🚀 1000))

(define s-emoji-10000 (repeat-char #\🚀 10000))

(define (run-benchmarks)
  (display "=== string->utf8 性能测试 ===\n\n")

  (display "--- ASCII 字符 (#\\a, 1 字节) ---\n")
  (bench "长度 1      " (lambda () (string->utf8 s-a-1)) 50000)
  (bench "长度 10     " (lambda () (string->utf8 s-a-10)) 10000)
  (bench "长度 100    " (lambda () (string->utf8 s-a-100)) 1000)
  (bench "长度 1000   " (lambda () (string->utf8 s-a-1000)) 100)
  (bench "长度 10000  " (lambda () (string->utf8 s-a-10000)) 10)
  (newline)

  (display "--- 拉丁字符 (#\\ä, 2 字节) ---\n")
  (bench "长度 1      " (lambda () (string->utf8 s-la-1)) 50000)
  (bench "长度 10     " (lambda () (string->utf8 s-la-10)) 10000)
  (bench "长度 100    " (lambda () (string->utf8 s-la-100)) 1000)
  (bench "长度 1000   " (lambda () (string->utf8 s-la-1000)) 100)
  (bench "长度 10000  " (lambda () (string->utf8 s-la-10000)) 10)
  (newline)

  (display "--- 中文字符 (#\\中, 3 字节) ---\n")
  (bench "长度 1      " (lambda () (string->utf8 s-zh-1)) 50000)
  (bench "长度 10     " (lambda () (string->utf8 s-zh-10)) 10000)
  (bench "长度 100    " (lambda () (string->utf8 s-zh-100)) 1000)
  (bench "长度 1000   " (lambda () (string->utf8 s-zh-1000)) 100)
  (bench "长度 10000  " (lambda () (string->utf8 s-zh-10000)) 10)
  (newline)

  (display "--- Emoji 字符 (#\\🚀, 4 字节) ---\n")
  (bench "长度 1      " (lambda () (string->utf8 s-emoji-1)) 50000)
  (bench "长度 10     " (lambda () (string->utf8 s-emoji-10)) 10000)
  (bench "长度 100    " (lambda () (string->utf8 s-emoji-100)) 1000)
  (bench "长度 1000   " (lambda () (string->utf8 s-emoji-1000)) 100)
  (bench "长度 10000  " (lambda () (string->utf8 s-emoji-10000)) 10)
  (newline)

  (display "--- start/end 切片 (1000 字符中文字符串) ---\n")
  (bench "全程 0 1000 " (lambda () (string->utf8 s-zh-1000 0 1000)) 100)
  (bench "前半 0 500  " (lambda () (string->utf8 s-zh-1000 0 500)) 100)
  (bench "后半 500 1000" (lambda () (string->utf8 s-zh-1000 500 1000)) 100)
  (bench "中段 200 800" (lambda () (string->utf8 s-zh-1000 200 800)) 100)
  (newline)
) ;define

(run-benchmarks)
