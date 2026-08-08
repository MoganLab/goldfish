;; string-prefix? / string-suffix? 性能基准测试
;; 对比旧 Scheme 实现（substring 分配临时串 + string=?）
;; 与新实现（复用 liii_string.cpp 的 g_string-starts?/g_string-ends?）

(import (liii timeit) (scheme base) (srfi srfi-13))

;; 旧 Scheme 实现（优化前 srfi-13 中的定义）

(define (string-prefix?-scheme prefix str)
  (let* ((prefix-len (string-length prefix)) (str-len (string-length str)))
    (and (<= prefix-len str-len) (string=? prefix (substring str 0 prefix-len)))
  ) ;let*
) ;define

(define (string-suffix?-scheme suffix str)
  (let* ((suffix-len (string-length suffix)) (str-len (string-length str)))
    (and (<= suffix-len str-len)
      (string=? suffix (substring str (- str-len suffix-len) str-len))
    ) ;and
  ) ;let*
) ;define

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
  (display "=== string-prefix? / string-suffix? 性能测试 ===\n\n")

  ;; 短字符串，短前缀/后缀（最常见场景：扩展名、路径判断）
  (bench "prefix? Scheme 短串匹配      "
    (lambda () (string-prefix?-scheme "doc" "document.txt"))
    100000
  ) ;bench
  (bench "prefix? C      短串匹配      "
    (lambda () (string-prefix? "doc" "document.txt"))
    100000
  ) ;bench
  (bench "prefix? Scheme 短串不匹配    "
    (lambda () (string-prefix?-scheme "txt" "document.txt"))
    100000
  ) ;bench
  (bench "prefix? C      短串不匹配    "
    (lambda () (string-prefix? "txt" "document.txt"))
    100000
  ) ;bench
  (bench "suffix? Scheme 短串匹配      "
    (lambda () (string-suffix?-scheme ".txt" "document.txt"))
    100000
  ) ;bench
  (bench "suffix? C      短串匹配      "
    (lambda () (string-suffix? ".txt" "document.txt"))
    100000
  ) ;bench
  (bench "suffix? Scheme 短串不匹配    "
    (lambda () (string-suffix?-scheme ".pdf" "document.txt"))
    100000
  ) ;bench
  (bench "suffix? C      短串不匹配    "
    (lambda () (string-suffix? ".pdf" "document.txt"))
    100000
  ) ;bench
  (newline)

  ;; 长字符串 + 长前缀/后缀完全匹配（旧实现要 substring + string=?）
  (let* ((long-str (make-string 1000 #\a))
         (prefix (make-string 100 #\a))
         (suffix (make-string 100 #\a))
        ) ;
    (bench "prefix? Scheme 长串匹配      "
      (lambda () (string-prefix?-scheme prefix long-str))
      100000
    ) ;bench
    (bench "prefix? C      长串匹配      "
      (lambda () (string-prefix? prefix long-str))
      100000
    ) ;bench
    (bench "suffix? Scheme 长串匹配      "
      (lambda () (string-suffix?-scheme suffix long-str))
      100000
    ) ;bench
    (bench "suffix? C      长串匹配      "
      (lambda () (string-suffix? suffix long-str))
      100000
    ) ;bench
  ) ;let*
  (newline)

  ;; 边界：空前缀/后缀、前缀比串长
  (bench "prefix? Scheme 空前缀        "
    (lambda () (string-prefix?-scheme "" "hello"))
    100000
  ) ;bench
  (bench "prefix? C      空前缀        "
    (lambda () (string-prefix? "" "hello"))
    100000
  ) ;bench
  (bench "prefix? Scheme 前缀过长      "
    (lambda () (string-prefix?-scheme "hello" "hi"))
    100000
  ) ;bench
  (bench "prefix? C      前缀过长      "
    (lambda () (string-prefix? "hello" "hi"))
    100000
  ) ;bench
  (bench "suffix? Scheme 后缀过长      "
    (lambda () (string-suffix?-scheme "hello" "hi"))
    100000
  ) ;bench
  (bench "suffix? C      后缀过长      "
    (lambda () (string-suffix? "hello" "hi"))
    100000
  ) ;bench
  (newline)

  ;; 中文 UTF-8 场景
  (bench "prefix? Scheme 中文匹配      "
    (lambda () (string-prefix?-scheme "中文" "中文测试字符串"))
    100000
  ) ;bench
  (bench "prefix? C      中文匹配      "
    (lambda () (string-prefix? "中文" "中文测试字符串"))
    100000
  ) ;bench
  (bench "suffix? Scheme 中文匹配      "
    (lambda () (string-suffix?-scheme "串" "中文测试字符串"))
    100000
  ) ;bench
  (bench "suffix? C      中文匹配      "
    (lambda () (string-suffix? "串" "中文测试字符串"))
    100000
  ) ;bench
) ;define

(run-benchmarks)
