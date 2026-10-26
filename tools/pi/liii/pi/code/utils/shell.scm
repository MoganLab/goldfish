;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 对齐上游 pi packages/coding-agent/src/utils/shell.ts 的 sanitizeBinaryOutput：
;; 清理命令输出中会导致显示错乱或宽度计算崩溃的字符。
;; 注意：本实现工作在字节层（与 (liii tui text-width core) 一致）。
(define-library (liii pi code utils shell)
  (export sanitize-binary-output)
  (import (scheme base) (liii error))
  (begin

    ;; 删除两类内容：
    ;; - C0 控制字符 0-8、11、12、14-31（保留 \t=9、\n=10、\r=13）
    ;; - U+FFF9..U+FFFB（UTF-8 为 EF BF B9..BB，interlinear annotation，
    ;;   会使宽度计算崩溃），三字节序列整体删除
    (define (sanitize-binary-output str)
      (unless (string? str)
        (type-error "sanitize-binary-output: str must be a string")
      ) ;unless
      (let* ((bv (string->utf8 str))
             (len (bytevector-length bv))
             (keep (make-bytevector len 0))
            ) ;
        (let loop
          ((i 0) (j 0))
          (if (>= i len)
            (utf8->string (if (= j len) keep (bytevector-copy keep 0 j)))
            (let ((b (bytevector-u8-ref bv i)))
              (cond
                ;; U+FFF9..U+FFFB：EF BF B9/BA/BB 整体删除
                ((and (< (+ i 2) len)
                   (= b 239)
                   (= (bytevector-u8-ref bv (+ i 1)) 191)
                   (let ((c (bytevector-u8-ref bv (+ i 2))))
                     (and (>= c 185) (<= c 187))
                   ) ;let
                 ) ;and
                 (loop (+ i 3) j)
                ) ;
                ;; 其余 C0 控制字符（保留 \t \n \r）
                ((and (< b 32) (not (= b 9)) (not (= b 10)) (not (= b 13))) (loop (+ i 1) j))
                (else (bytevector-u8-set! keep j b) (loop (+ i 1) (+ j 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
