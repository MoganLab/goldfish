;; utf8-string-ref 性能基准：C 原语实现 vs 纯 Scheme 实现
;;
;; 用法：bin/gf bench/utf8-string-ref-perf.scm
;;
;; 对照组 %scheme-utf8-string-ref 是优化前的纯 Scheme 实现逻辑，复制自
;; r7rs/scheme/base.scm 的 utf8-string-length 风格 + boot.scm 的
;; utf8->codepoint-at，用 string->byte-vector + bytevector-advance-utf8
;; + 循环定位。

(import (liii timeit) (liii unicode) (scheme base) (scheme write))

;; ---- 纯 Scheme 对照实现（优化前逻辑） ----
(define (%scheme-utf8-string-ref str index)
  (let ((bv (string->byte-vector str)) (N (string-length str)))
    (let loop
      ((pos 0) (i 0))
      (if (>= pos N)
        (error 'out-of-range "utf8-string-ref: index out of range" index)
        (let ((next-pos (bytevector-advance-utf8 bv pos N)))
          (cond ((= next-pos pos) (error 'value-error "Invalid UTF-8 sequence at index: " pos))
                ((= i index) (integer->char (utf8->codepoint-at bv pos)))
                (else (loop next-pos (+ i 1)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;let
) ;define

(define (bench name thunk number)
  (let ((elapsed (timeit thunk '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

;; 构造测试串：长字符串，分别取首、中、尾三个位置
(define ascii-str (make-string 10000 #\a))
(define cn-str (utf8-make-string 10000 #\中))
(define emoji-str (utf8-make-string 10000 #\👍))

(display "=== utf8-string-ref 性能对比 (C 原语 vs 纯 Scheme) ===\n\n")

(display "--- ASCII 串 (10000 字符, 取末位 index 9999, 最坏遍历) ---\n")
(bench "C 原语   " (lambda () (utf8-string-ref ascii-str 9999)) 1000)
(bench "纯Scheme" (lambda () (%scheme-utf8-string-ref ascii-str 9999)) 1000)
(newline)

(display "--- 中文串 (10000 码点 = 30000 字节, 取末位 index 9999) ---\n")
(bench "C 原语   " (lambda () (utf8-string-ref cn-str 9999)) 1000)
(bench "纯Scheme" (lambda () (%scheme-utf8-string-ref cn-str 9999)) 1000)
(newline)

(display "--- Emoji 串 (10000 码点 = 40000 字节, 取末位 index 9999) ---\n")
(bench "C 原语   " (lambda () (utf8-string-ref emoji-str 9999)) 1000)
(bench "纯Scheme" (lambda () (%scheme-utf8-string-ref emoji-str 9999)) 1000)
(newline)

(display "--- 中文串 取首位 index 0 (最好情况, 不需遍历) ---\n")
(bench "C 原语   " (lambda () (utf8-string-ref cn-str 0)) 10000)
(bench "纯Scheme" (lambda () (%scheme-utf8-string-ref cn-str 0)) 10000)
(newline)

(display "--- 中文串 取中间 index 5000 ---\n")
(bench "C 原语   " (lambda () (utf8-string-ref cn-str 5000)) 1000)
(bench "纯Scheme" (lambda () (%scheme-utf8-string-ref cn-str 5000)) 1000)
(newline)
