;; gzip-decompress
;; 解压 gzip 容器（RFC 1952）数据，支持多成员拼接流。
;;
;; 语法
;; ----
;; (gzip-decompress bv)
;; (gzip-decompress bv #:window-bits W)
;;
;; 参数
;; ----
;; bv : bytevector?
;; gzip 容器数据，可含多个拼接的 gzip 成员。
;; window-bits : 8..15，默认 15（解压侧窗口须 ≥ 压缩侧）。
;;
;; 返回值
;; -----
;; bytevector?
;; 解压后的原始字节向量。
;;
;; 说明
;; ----
;; 1. 与 gzip-compress 往返恒等。
;; 2. 支持多成员拼接流（多个 gzip 流首尾相接）。
;; 3. 损坏 / 截断流抛 zlib-error（消息含 zlib msg 与错误码）。
;; 4. 解压输出上限 512MB，超限抛 zlib-error。

(import (liii check) (liii zlib) (liii unicode) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; 往返
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "hello" 6))) "hello")
  =>
  #t
) ;check
(check (equal? (gzip-decompress (gzip-compress #u8(1 2 3 4 5) 6)) #u8(1 2 3 4 5))
  =>
  #t
) ;check

;; 多成员拼接流

(define (bytevector-append2 a b)
  (let ((r (make-bytevector (+ (bytevector-length a) (bytevector-length b)) 0)))
    (bytevector-copy! r 0 a 0 (bytevector-length a))
    (bytevector-copy! r (bytevector-length a) b 0 (bytevector-length b))
    r
  ) ;let
) ;define

(define multi
  (bytevector-append2 (gzip-compress "foo" 6) (gzip-compress "bar" 6))
) ;define
(check (equal? (utf8->string (gzip-decompress multi)) "foobar") => #t)

;; 损坏数据 / 截断流 / 非法容器
(check-catch 'zlib-error (gzip-decompress #u8(1 2 3 4 5 6 7 8 9 10)))
(check-catch 'zlib-error (gzip-decompress (bytevector 31 139 8 0 0 0 0 0 0 10)))
;; 截断的 gzip 流：去掉末尾 4 字节（CRC + 长度）
(let ((full (gzip-compress "hello" 6)))
  (check-catch 'zlib-error
    (gzip-decompress (bytevector-copy full 0 (- (bytevector-length full) 4)))
  ) ;check-catch
) ;let
(check-catch 'zlib-error (gzip-decompress (deflate-compress "x" 6)))

;; 类型检查
(check-catch 'type-error (gzip-decompress "not a bytevector"))

(check-report)
