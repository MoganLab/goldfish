;; deflate-compress
;; 将数据压缩为 zlib 容器（RFC 1950）格式，对齐 Python zlib.compress。
;;
;; 语法
;; ----
;; (deflate-compress data)
;; (deflate-compress data level)
;;
;; 参数
;; ----
;; data : string? 或 bytevector?
;; 待压缩数据，按原始字节处理（string 不做编码转换）。
;; level : -1 或 0..9，默认 -1（Z_DEFAULT_COMPRESSION）。
;;
;; 返回值
;; -----
;; bytevector?
;; zlib 容器字节向量，首字节为 #x78（zlib 头标志，CM=8）。
;;
;; 说明
;; ----
;; 1. 同级别 zlib 实现的输出逐字节确定，与 Python zlib.compress 一致
;;    （deflate-compress "123456789" 6 的输出见下用例）。
;; 2. 与 deflate-decompress 往返恒等；与 decompress（auto）往返恒等。
;; 3. 把 gzip 容器交给 deflate-decompress 会抛 zlib-error（格式严格）。

(import (liii check) (liii zlib) (liii unicode) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; zlib 容器头标志 #x78
(check (bytevector-u8-ref (deflate-compress "x" 6) 0) => 120)

;; 与 Python zlib.compress 逐字节一致的固定样本
(check (deflate-compress "" 6) => #u8(120 156 3 0 0 0 0 1))
(check (deflate-compress "123456789" 6)
  =>
  #u8(120 156 51 52 50 54 49 53 51 183 176 4 0 9 30 1 222)
) ;check

;; 往返恒等

(define (roundtrip data)
  (equal? (utf8->string (deflate-decompress (deflate-compress data 6))) data)
) ;define
(check (roundtrip "hello") => #t)
(check (roundtrip "") => #t)
(check (roundtrip "The quick brown fox jumps over the lazy dog") => #t)
(check
  (equal? (deflate-decompress (deflate-compress (bytevector 1 2 3 4 5) 6))
    (bytevector 1 2 3 4 5)
  ) ;equal?
  =>
  #t
) ;check
(check (roundtrip (make-string 4096 #\a)) => #t)

;; level 取值
(check
  (equal? (utf8->string (deflate-decompress (deflate-compress "data" -1))) "data")
  =>
  #t
) ;check
(check
  (equal? (utf8->string (deflate-decompress (deflate-compress "data" 0))) "data")
  =>
  #t
) ;check
(check
  (equal? (utf8->string (deflate-decompress (deflate-compress "data" 9))) "data")
  =>
  #t
) ;check

;; 格式严格：gzip 容器交给 deflate-decompress 应失败
(check-catch 'zlib-error (deflate-decompress (gzip-compress "x" 6)))

;; 非法参数
(check-catch 'value-error (deflate-compress "x" 10))
(check-catch 'type-error (deflate-compress 123 6))

(check-report)
