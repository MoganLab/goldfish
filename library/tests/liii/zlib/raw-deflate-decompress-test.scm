;; raw-deflate-decompress
;; 解压裸 DEFLATE（RFC 1951）流，对齐 Python zlib.decompress(wbits=-15)。
;;
;; 语法
;; ----
;; (raw-deflate-decompress bv)
;; (raw-deflate-decompress bv #:window-bits W)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 裸 DEFLATE 数据。
;; window-bits : 8..15，默认 15。
;;
;; 返回值
;; -----
;; bytevector?
;; 解压后的原始字节向量。
;;
;; 说明
;; ----
;; 1. 与 raw-deflate-compress 往返恒等。
;; 2. 裸流无成员边界语义，只解单流；流后多余输入视为垃圾抛 zlib-error。
;; 3. WebSocket permessage-deflate 帧需在调用前去/补 00 00 FF FF 同步标记。

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
  (equal? (utf8->string (raw-deflate-decompress (raw-deflate-compress "hello" 6)))
    "hello"
  ) ;equal?
  =>
  #t
) ;check
(check (equal? (raw-deflate-decompress (raw-deflate-compress #u8(1 2 3 4 5) 6))
         #u8(1 2 3 4 5)
       ) ;equal?
  =>
  #t
) ;check

;; 固定样本（与 Python zlib.compressobj(wbits=-15) 一致）
(check (equal? (raw-deflate-decompress #u8(3 0)) #u8()) => #t)
(check (equal? (utf8->string (raw-deflate-decompress #u8(75 76 74 6 0))) "abc")
  =>
  #t
) ;check

;; RFC 7692 帧：去 00 00 FF FF 尾后是裸流，可直接解压
(let* ((ws-frame (raw-deflate-compress "hello" 6))
       (ws-frame (bytevector-append2 ws-frame #u8(0 0 255 255)))
      ) ;
  (check
    (equal?
      (utf8->string
        (raw-deflate-decompress (bytevector-copy ws-frame 0 (- (bytevector-length ws-frame) 4))
        ) ;raw-deflate-decompress
      ) ;utf8->string
      "hello"
    ) ;equal?
    =>
    #t
  ) ;check
) ;let*

;; 格式严格：zlib/gzip 容器交给裸流解压应失败
(check-catch 'zlib-error (raw-deflate-decompress (deflate-compress "x" 6)))
(check-catch 'zlib-error (raw-deflate-decompress (gzip-compress "x" 6)))

;; 损坏
(check-catch 'zlib-error (raw-deflate-decompress #u8(1 2 3 4 5 6 7 8 9 10)))

;; 流后多余输入视为垃圾
(let ((full (raw-deflate-compress "hi" 6)))
  (check-catch 'zlib-error
    (raw-deflate-decompress (bytevector-append2 full #u8(99)))
  ) ;check-catch
) ;let

;; 类型检查
(check-catch 'type-error (raw-deflate-decompress "not a bytevector"))

(check-report)
