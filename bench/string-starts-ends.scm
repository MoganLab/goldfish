;; string-starts? / string-ends? 性能基准测试
;; 对比旧 Scheme 实现（基于 srfi-13 string-prefix?/string-suffix?）
;; 与新 C 实现（liii_string.cpp 的 g_string-starts?/g_string-ends?）

(import (liii timeit) (liii string) (scheme base) (srfi srfi-13))

;; 旧 Scheme 实现（优化前 liii string 中的定义）

(define (string-starts?-scheme str prefix)
  (if (and (string? str) (string? prefix))
    (string-prefix? prefix str)
    (error 'type-error "string-starts? parameter is not a string")
  ) ;if
) ;define

(define (string-ends?-scheme str suffix)
  (if (and (string? str) (string? suffix))
    (string-suffix? suffix str)
    (error 'type-error "string-ends? parameter is not a string")
  ) ;if
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
  (display "=== string-starts? / string-ends? 性能测试 ===\n\n")

  ;; 短字符串，短前缀/后缀（最常见场景：扩展名、路径判断）
  (bench "starts? Scheme 短串匹配      "
    (lambda () (string-starts?-scheme "document.txt" "doc"))
    100000
  ) ;bench
  (bench "starts? C      短串匹配      "
    (lambda () (string-starts? "document.txt" "doc"))
    100000
  ) ;bench
  (bench "starts? Scheme 短串不匹配    "
    (lambda () (string-starts?-scheme "document.txt" "txt"))
    100000
  ) ;bench
  (bench "starts? C      短串不匹配    "
    (lambda () (string-starts? "document.txt" "txt"))
    100000
  ) ;bench
  (bench "ends?   Scheme 短串匹配      "
    (lambda () (string-ends?-scheme "document.txt" ".txt"))
    100000
  ) ;bench
  (bench "ends?   C      短串匹配      "
    (lambda () (string-ends? "document.txt" ".txt"))
    100000
  ) ;bench
  (bench "ends?   Scheme 短串不匹配    "
    (lambda () (string-ends?-scheme "document.txt" ".pdf"))
    100000
  ) ;bench
  (bench "ends?   C      短串不匹配    "
    (lambda () (string-ends? "document.txt" ".pdf"))
    100000
  ) ;bench
  (newline)

  ;; 长字符串 + 长前缀/后缀完全匹配（旧实现要 substring + string=?）
  (let* ((long-str (make-string 1000 #\a))
         (prefix (make-string 100 #\a))
         (suffix (make-string 100 #\a))
        ) ;
    (bench "starts? Scheme 长串匹配      "
      (lambda () (string-starts?-scheme long-str prefix))
      100000
    ) ;bench
    (bench "starts? C      长串匹配      "
      (lambda () (string-starts? long-str prefix))
      100000
    ) ;bench
    (bench "ends?   Scheme 长串匹配      "
      (lambda () (string-ends?-scheme long-str suffix))
      100000
    ) ;bench
    (bench "ends?   C      长串匹配      "
      (lambda () (string-ends? long-str suffix))
      100000
    ) ;bench
  ) ;let*
  (newline)

  ;; 边界：空前缀/后缀、前缀比串长
  (bench "starts? Scheme 空前缀        "
    (lambda () (string-starts?-scheme "hello" ""))
    100000
  ) ;bench
  (bench "starts? C      空前缀        "
    (lambda () (string-starts? "hello" ""))
    100000
  ) ;bench
  (bench "starts? Scheme 前缀过长      "
    (lambda () (string-starts?-scheme "hi" "hello"))
    100000
  ) ;bench
  (bench "starts? C      前缀过长      "
    (lambda () (string-starts? "hi" "hello"))
    100000
  ) ;bench
  (bench "ends?   Scheme 后缀过长      "
    (lambda () (string-ends?-scheme "hi" "hello"))
    100000
  ) ;bench
  (bench "ends?   C      后缀过长      "
    (lambda () (string-ends? "hi" "hello"))
    100000
  ) ;bench
  (newline)

  ;; 中文 UTF-8 场景
  (bench "starts? Scheme 中文匹配      "
    (lambda () (string-starts?-scheme "中文测试字符串" "中文"))
    100000
  ) ;bench
  (bench "starts? C      中文匹配      "
    (lambda () (string-starts? "中文测试字符串" "中文"))
    100000
  ) ;bench
  (bench "ends?   Scheme 中文匹配      "
    (lambda () (string-ends?-scheme "中文测试字符串" "串"))
    100000
  ) ;bench
  (bench "ends?   C      中文匹配      "
    (lambda () (string-ends? "中文测试字符串" "串"))
    100000
  ) ;bench
) ;define

(run-benchmarks)
