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

    ;; 是否存在需要删除的字节：
    ;; - C0 控制字符 0-8、11、12、14-31（\t=9、\n=10、\r=13 除外）
    ;; - U+FFF9..U+FFFB（UTF-8 为 EF BF B9..BB，interlinear annotation，
    ;;   会使宽度计算崩溃）
    (define (%bv-needs-clean? bv len)
      (let loop
        ((i 0))
        (if (>= i len)
          #f
          (let ((b (bytevector-u8-ref bv i)))
            (cond
             ((and (< b 32) (not (= b 9)) (not (= b 10)) (not (= b 13))) #t)
             ((and (= b 239)
                (< (+ i 2) len)
                (= (bytevector-u8-ref bv (+ i 1)) 191)
                (let ((c (bytevector-u8-ref bv (+ i 2))))
                  (and (>= c 185) (<= c 187))
                ) ;let
              ) ;and
              #t
             ) ;
             (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 单遍复制需要保留的字节到 keep，返回实际保留长度 j
    (define (%copy-clean-bytes! bv len keep)
      (let loop
        ((i 0) (j 0))
        (if (>= i len)
          j
          (let ((b (bytevector-u8-ref bv i)))
            (cond
              ;; U+FFF9..U+FFFB：EF BF B9/BA/BB 三字节整体删除
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
    ) ;define

    ;; 快速路径：无字节需要删除时直接返回原串，避免分配重建
    ;; （渲染层每帧都会调用，纯文本输出不能付出重建代价）
    (define (sanitize-binary-output str)
      (unless (string? str)
        (type-error "sanitize-binary-output: str must be a string")
      ) ;unless
      (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
        (if (%bv-needs-clean? bv len)
          (let* ((keep (make-bytevector len 0)) (j (%copy-clean-bytes! bv len keep)))
            (utf8->string (if (= j len) keep (bytevector-copy keep 0 j)))
          ) ;let*
          str
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
