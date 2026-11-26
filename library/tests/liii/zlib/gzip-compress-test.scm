;; gzip-compress
;; 将数据压缩为 gzip 容器（RFC 1952）格式。
;;
;; 语法
;; ----
;; (gzip-compress data)
;; (gzip-compress data level)
;; (gzip-compress data level #:window-bits W #:strategy S #:mem-level M)
;;
;; 参数
;; ----
;; data : string? 或 bytevector?
;; 待压缩数据，按原始字节处理（string 不做编码转换）。
;; level : -1 或 0..9
;; 压缩级别，-1 表示默认（Z_DEFAULT_COMPRESSION，约 6）。默认 -1。
;; window-bits : 8..15，默认 15。
;; strategy : 'default/'filtered/'huffman-only/'rle/'fixed，默认 'default。
;; mem-level : 1..9，默认 8。
;;
;; 返回值
;; -----
;; bytevector?
;; gzip 容器字节向量，首两字节为 gzip 魔数 #x1F #x8B。
;;
;; 说明
;; ----
;; 1. 解压输出恒为 bytevector，文本需调用方 utf8->string。
;; 2. 与 gzip-decompress 往返恒等，与 decompress（auto）往返恒等。
;; 3. level 非 -1/0..9 抛 value-error；data 类型不符抛 type-error。

(import (liii check) (liii zlib) (liii unicode) (liii error) (scheme base))

(check-set-mode! 'report-failed)

(define (roundtrip data)
  (equal? (utf8->string (gzip-decompress (gzip-compress data 6))) data)
) ;define

;; gzip 魔数
(check (bytevector-u8-ref (gzip-compress "x" 6) 0) => 31)
(check (bytevector-u8-ref (gzip-compress "x" 6) 1) => 139)

;; 往返：string / bytevector / 空 / 较大输入
(check (roundtrip "hello") => #t)
(check (roundtrip "") => #t)
(check (roundtrip "The quick brown fox jumps over the lazy dog") => #t)
(check
  (equal? (gzip-decompress (gzip-compress (bytevector 1 2 3 4 5) 6))
    (bytevector 1 2 3 4 5)
  ) ;equal?
  =>
  #t
) ;check
(check (roundtrip (make-string 1024 #\a)) => #t)

;; level 取值
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "data" -1))) "data")
  =>
  #t
) ;check
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "data" 0))) "data")
  =>
  #t
) ;check
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "data" 9))) "data")
  =>
  #t
) ;check

;; 关键字可选参数
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "data" 6 :window-bits 9)))
    "data"
  ) ;equal?
  =>
  #t
) ;check
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "data" 6 :mem-level 3)))
    "data"
  ) ;equal?
  =>
  #t
) ;check
(check
  (equal? (utf8->string (gzip-decompress (gzip-compress "data" 6 :strategy 'fixed)))
    "data"
  ) ;equal?
  =>
  #t
) ;check

;; 非法参数
(check-catch 'value-error (gzip-compress "x" 10))
(check-catch 'value-error (gzip-compress "x" -2))
(check-catch 'value-error (gzip-compress "x" 6 :window-bits 7))
(check-catch 'value-error (gzip-compress "x" 6 :mem-level 0))
(check-catch 'type-error (gzip-compress 123 6))

(check-report)
