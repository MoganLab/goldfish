;; string-ref/cursor 性能基准测试
;; 测试当前实现中 bytevector-copy + utf8->codepoint 的性能开销

(import (liii timeit)
        (liii string-cursor)
        (scheme base)
)

;; 运行单次 benchmark
(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  )
)

;; 使用 string-ref/cursor 遍历字符串的所有字符
(define (cursor-traverse str)
  (let ((end (string-cursor-end str)))
    (let loop ((cur (string-cursor-start str)))
      (if (string-cursor=? cur end)
        'done
        (begin
          (string-ref/cursor str cur)
          (loop (string-cursor-next str cur))
        )
      )
    )
  )
)

;; 使用原生 string-ref 遍历字符串的所有字符
(define (native-traverse str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (= i len)
        'done
        (begin
          (string-ref str i)
          (loop (+ i 1))
        )
      )
    )
  )
)

;; 使用 string-fold 遍历（内部使用 string-ref/cursor）
(define (fold-traverse str)
  (string-fold (lambda (ch acc) (+ acc 1)) 0 str)
)

;; 使用 string-every 遍历（内部使用 string-ref/cursor）
(define (every-traverse str)
  (string-every char? str)
)

(define (run-benchmarks)
  (display "=== string-ref/cursor 性能测试 ===\n\n")

  ;; ASCII 短字符串（50 字符）
  (let ((ascii-short (make-string 50 #\a)))
    (display "--- ASCII 短字符串 (50 字符) ---\n")
    (bench "  cursor 遍历   " (lambda () (cursor-traverse ascii-short)) 10000)
    (bench "  native 遍历   " (lambda () (native-traverse ascii-short)) 10000)
    (bench "  string-fold   " (lambda () (fold-traverse ascii-short)) 10000)
    (bench "  string-every  " (lambda () (every-traverse ascii-short)) 10000)
    (display "\n")
  )

  ;; ASCII 长字符串（500 字符）
  (let ((ascii-long (make-string 500 #\a)))
    (display "--- ASCII 长字符串 (500 字符) ---\n")
    (bench "  cursor 遍历   " (lambda () (cursor-traverse ascii-long)) 1000)
    (bench "  native 遍历   " (lambda () (native-traverse ascii-long)) 1000)
    (bench "  string-fold   " (lambda () (fold-traverse ascii-long)) 1000)
    (bench "  string-every  " (lambda () (every-traverse ascii-long)) 1000)
    (display "\n")
  )

  ;; 混合 UTF-8 短字符串（20 个汉字，每个 3 字节）
  (let ((utf8-short (string-tabulate (lambda (i) #\中) 20)))
    (display "--- UTF-8 短字符串 (20 个汉字) ---\n")
    (bench "  cursor 遍历   " (lambda () (cursor-traverse utf8-short)) 10000)
    (bench "  native 遍历   " (lambda () (native-traverse utf8-short)) 10000)
    (bench "  string-fold   " (lambda () (fold-traverse utf8-short)) 10000)
    (bench "  string-every  " (lambda () (every-traverse utf8-short)) 10000)
    (display "\n")
  )

  ;; 混合 UTF-8 长字符串（200 个汉字）
  (let ((utf8-long (string-tabulate (lambda (i) #\中) 200)))
    (display "--- UTF-8 长字符串 (200 个汉字) ---\n")
    (bench "  cursor 遍历   " (lambda () (cursor-traverse utf8-long)) 1000)
    (bench "  native 遍历   " (lambda () (native-traverse utf8-long)) 1000)
    (bench "  string-fold   " (lambda () (fold-traverse utf8-long)) 1000)
    (bench "  string-every  " (lambda () (every-traverse utf8-long)) 1000)
    (display "\n")
  )

  ;; Emoji 短字符串（10 个 emoji，每个 4 字节）
  (let ((emoji-short (string-tabulate (lambda (i) #\🚀) 10)))
    (display "--- Emoji 短字符串 (10 个 emoji) ---\n")
    (bench "  cursor 遍历   " (lambda () (cursor-traverse emoji-short)) 10000)
    (bench "  native 遍历   " (lambda () (native-traverse emoji-short)) 10000)
    (bench "  string-fold   " (lambda () (fold-traverse emoji-short)) 10000)
    (bench "  string-every  " (lambda () (every-traverse emoji-short)) 10000)
    (display "\n")
  )
)

(run-benchmarks)
