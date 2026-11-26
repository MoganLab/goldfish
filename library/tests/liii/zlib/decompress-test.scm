;; decompress
;; 自动识别 gzip / zlib 容器并解压（对齐 Python zlib.decompress(MAX_WBITS|32)）。
;;
;; 语法
;; ----
;; (decompress bv)
;; (decompress bv #:window-bits W)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 压缩数据，自动识别 gzip 容器（RFC 1952）或 zlib 容器（RFC 1950）。
;; window-bits : 8..15，默认 15。
;;
;; 返回值
;; -----
;; bytevector?
;; 解压后的原始字节向量。
;;
;; 说明
;; ----
;; 1. gzip 容器与 zlib 容器均可解压。
;; 2. 裸 DEFLATE 流（无容器头）抛 zlib-error（裸流需用 raw-deflate-decompress）。
;; 3. gzip 多成员拼接流同样支持。

(import (liii check) (liii zlib) (liii unicode) (liii error) (scheme base))

(check-set-mode! 'report-failed)

(define (bytevector-append2 a b)
  (let ((r (make-bytevector (+ (bytevector-length a) (bytevector-length b)) 0)))
    (bytevector-copy! r 0 a 0 (bytevector-length a))
    (bytevector-copy! r (bytevector-length a) b 0 (bytevector-length b))
    r
  ) ;let
) ;define

;; gzip 容器
(check
  (equal? (utf8->string (decompress (gzip-compress "hello" 6))) "hello")
  =>
  #t
) ;check
;; zlib 容器
(check
  (equal? (utf8->string (decompress (deflate-compress "hello" 6))) "hello")
  =>
  #t
) ;check

;; 多成员 gzip 拼接流

(define multi
  (bytevector-append2 (gzip-compress "foo" 6) (gzip-compress "bar" 6))
) ;define
(check (equal? (utf8->string (decompress multi)) "foobar") => #t)

;; 裸流不可识别（无容器头）
(check-catch 'zlib-error (decompress (raw-deflate-compress "x" 6)))

;; 损坏
(check-catch 'zlib-error (decompress #u8(1 2 3 4 5 6 7 8 9 10)))

;; 类型检查
(check-catch 'type-error (decompress "not a bytevector"))

(check-report)
