;; string-split 性能基准测试
;; 测试 (liii string) 中 string-split 的性能，为 C++ 实现提供基准数据

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
  (display "=== string-split 性能测试 ===\n\n")

  (bench "短字符串(3字段) 逗号分隔   "
    (lambda () (string-split "a,b,c" ","))
    100000
  ) ;bench

  (bench "中字符串(10字段) 逗号分隔  "
    (lambda () (string-split "1,2,3,4,5,6,7,8,9,10" ","))
    100000
  ) ;bench

  (let ((long-csv (let loop
                    ((i 0) (parts '()))
                    (if (= i 100)
                      (apply string-append (reverse (cdr (reverse parts))))
                      (loop (+ i 1) (cons (string-append (number->string i) ",") parts))
                    ) ;if
                  ) ;let
        ) ;long-csv
       ) ;
    (bench "长字符串(100字段) 逗号分隔 "
      (lambda () (string-split long-csv ","))
      10000
    ) ;bench
  ) ;let

  (bench "多字符分隔符(::)          "
    (lambda () (string-split "path::to::file::name" "::"))
    100000
  ) ;bench

  (bench "字符分隔符(#\\,)           "
    (lambda () (string-split "a,b,c,d,e" #\,))
    100000
  ) ;bench

  (bench "空分隔符(按字符拆分)       "
    (lambda () (string-split "hello world" ""))
    100000
  ) ;bench

  (bench "空分隔符(中文字符拆分)     "
    (lambda () (string-split "你好世界金金鱼" ""))
    100000
  ) ;bench

  (bench "多字节分隔符(，)           "
    (lambda () (string-split "你好，世界，Goldfish，Scheme" "，"))
    100000
  ) ;bench
) ;define

(run-benchmarks)
