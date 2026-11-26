;; raw-deflate-compress
;; 将数据压缩为裸 DEFLATE（RFC 1951）流，不含容器头/尾，WS permessage-deflate 用。
;;
;; 语法
;; ----
;; (raw-deflate-compress data)
;; (raw-deflate-compress data level)
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
;; 裸 DEFLATE 字节向量，无 zlib 头（首字节不为 #x78）。
;;
;; 说明
;; ----
;; 1. 产物可直接用于 WebSocket permessage-deflate 上下文（去掉/补回 00 00 FF FF 同步标记）。
;; 2. 与 raw-deflate-decompress 往返恒等。
;; 3. 与 Python zlib.compressobj(wbits=-15) 输出逐字节一致（见下用例）。

(import (liii check) (liii zlib) (liii unicode) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; 裸流不含 zlib 头：首字节随压缩内容变化，但 "x" level 6 的输出是确定的。
;; 关键属性见后面 not #x78 断言。

;; 与 Python zlib.compressobj(wbits=-15) 逐字节一致的固定样本
(check (raw-deflate-compress "" 6) => #u8(3 0))
(check (raw-deflate-compress "abc" 6) => #u8(75 76 74 6 0))

;; 往返恒等

(define (roundtrip data)
  (equal? (utf8->string (raw-deflate-decompress (raw-deflate-compress data 6)))
    data
  ) ;equal?
) ;define
(check (roundtrip "hello") => #t)
(check (roundtrip "") => #t)
(check (roundtrip "hello world") => #t)
(check
  (equal? (raw-deflate-decompress (raw-deflate-compress (bytevector 1 2 3 4 5) 6))
    (bytevector 1 2 3 4 5)
  ) ;equal?
  =>
  #t
) ;check
(check (roundtrip (make-string 4096 #\a)) => #t)

;; level 取值
(check
  (equal? (utf8->string (raw-deflate-decompress (raw-deflate-compress "data" -1)))
    "data"
  ) ;equal?
  =>
  #t
) ;check
(check
  (equal? (utf8->string (raw-deflate-decompress (raw-deflate-compress "data" 0)))
    "data"
  ) ;equal?
  =>
  #t
) ;check
(check
  (equal? (utf8->string (raw-deflate-decompress (raw-deflate-compress "data" 9)))
    "data"
  ) ;equal?
  =>
  #t
) ;check

;; 裸流首字节确认不是 zlib 头 #x78
(check
  (not (= (bytevector-u8-ref (raw-deflate-compress "data" 6) 0) 120))
  =>
  #t
) ;check

;; 非法参数
(check-catch 'value-error (raw-deflate-compress "x" 10))
(check-catch 'type-error (raw-deflate-compress 123 6))

(check-report)
