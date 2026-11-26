;; deflate-decompress
;; 解压 zlib 容器（RFC 1950）数据，对齐 Python zlib.decompress 默认。
;;
;; 语法
;; ----
;; (deflate-decompress bv)
;; (deflate-decompress bv #:window-bits W)
;;
;; 参数
;; ----
;; bv : bytevector?
;; zlib 容器数据。
;; window-bits : 8..15，默认 15。
;;
;; 返回值
;; -----
;; bytevector?
;; 解压后的原始字节向量。
;;
;; 说明
;; ----
;; 1. 与 deflate-compress 往返恒等。
;; 2. zlib 单流：流后多余输入视为垃圾，抛 zlib-error。
;; 3. gzip 容器或裸流交给 deflate-decompress 抛 zlib-error（格式严格）。

(import (liii check) (liii zlib) (liii unicode) (liii error) (scheme base))

(check-set-mode! 'report-failed)

(define (bytevector-append2 a b)
  (let ((r (make-bytevector (+ (bytevector-length a) (bytevector-length b)) 0)))
    (bytevector-copy! r 0 a 0 (bytevector-length a))
    (bytevector-copy! r (bytevector-length a) b 0 (bytevector-length b))
    r
  ) ;let
) ;define

;; 往返
(check
  (equal? (utf8->string (deflate-decompress (deflate-compress "hello" 6)))
    "hello"
  ) ;equal?
  =>
  #t
) ;check
(check (equal? (deflate-decompress (deflate-compress #u8(1 2 3 4 5) 6)) #u8(1 2 3 4 5))
  =>
  #t
) ;check

;; 固定样本解压（与 Python zlib.decompress 一致）
(check (equal? (deflate-decompress #u8(120 156 3 0 0 0 0 1)) #u8()) => #t)
(check (equal? (utf8->string (deflate-decompress #u8(120 156 51 52 50 54 49 53 51 183 176 4 0 9 30 1 222))
               ) ;utf8->string
         "123456789"
       ) ;equal?
  =>
  #t
) ;check

;; 格式严格
(check-catch 'zlib-error (deflate-decompress (gzip-compress "x" 6)))
(check-catch 'zlib-error (deflate-decompress (raw-deflate-compress "x" 6)))

;; 损坏 / 截断
(check-catch 'zlib-error (deflate-decompress #u8(1 2 3 4 5 6 7 8 9 10)))
(check-catch 'zlib-error (deflate-decompress #u8(120 156 3 0 0 0 0)))

;; 流后多余输入视为垃圾
(let ((full (deflate-compress "hi" 6)))
  (check-catch 'zlib-error (deflate-decompress (bytevector-append2 full #u8(99))))
) ;let

;; 类型检查
(check-catch 'type-error (deflate-decompress "not a bytevector"))

(check-report)
