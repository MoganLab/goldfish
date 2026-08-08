;; string-replace 性能基准测试
;; 测试 (liii string) 中 string-replace 的性能

(import (liii timeit) (liii string) (scheme base))

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

(define (run-benchmarks)
  (display "=== string-replace 性能测试 ===\n\n")

  (bench "短字符串 单次替换          "
    (lambda () (string-replace "hello world" "world" "Goldfish"))
    100000
  ) ;bench

  (bench "短字符串 多次替换          "
    (lambda () (string-replace "hello world hello" "hello" "hi"))
    100000
  ) ;bench

  (bench "短字符串 无匹配            "
    (lambda () (string-replace "hello world" "test" "hi"))
    100000
  ) ;bench

  (bench "短字符串 count=1          "
    (lambda () (string-replace "hello hello hello" "hello" "hi" 1))
    100000
  ) ;bench

  (bench "短字符串 删除替换(new为空) "
    (lambda () (string-replace "hello world hello" "hello" ""))
    100000
  ) ;bench

  (bench "空pattern 插入            "
    (lambda () (string-replace "hello" "" "x"))
    100000
  ) ;bench

  (bench "中文字符串 替换           "
    (lambda () (string-replace "测试测试字符串" "测试" "实验"))
    100000
  ) ;bench

  (bench "Emoji 替换                "
    (lambda () (string-replace "hello😀world😀" "😀" "!"))
    100000
  ) ;bench

  (let ((long-str (string-join (map number->string (iota 1000)) ",")))
    (bench "长字符串(约4000字符) 无匹配   "
      (lambda () (string-replace long-str "not-found" "x"))
      10000
    ) ;bench
    (bench "长字符串(约4000字符) 多次替换 "
      (lambda () (string-replace long-str "," ";"))
      10000
    ) ;bench
  ) ;let
) ;define

(run-benchmarks)
