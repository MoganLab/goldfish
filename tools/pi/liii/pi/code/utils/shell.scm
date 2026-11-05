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
  (import (scheme base)
    (liii base)
    (liii error)
    (liii unicode)
    (liii tui text-width core)
  ) ;import
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
             ((or (= b 127) (= b 27)) #t)
             ((and (= b 194)
                (< (+ i 1) len)
                (>= (bytevector-u8-ref bv (+ i 1)) 128)
                (<= (bytevector-u8-ref bv (+ i 1)) 159)
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
    ;;
    ;; 注：子进程输出可能含非法 UTF-8 字节（被截断的多字节序列、二进制垃圾），
    ;; string->utf8 / utf8->string 遇非法字节会抛 value-error 使整段输出丢失；
    ;; 因此这里走字节层（string->byte-vector）并用 utf8-replace-invalid
    ;; 把非法序列替换为 U+FFFD（对齐上游 JS TextDecoder 容错行为）。
    ;; 先剥离完整终端控制序列，再删除其余控制字符。
    ;; 上游 937538437 的关键防护：strip-ansi 未识别的 ESC+字符也不能
    ;; 只删除 ESC 而把终结字节留给正文；RIS 的 ESC c 即属于这一类。
    (define (%copy-safe-escape-sequence! bv i len keep j)
      (let ((end (%match-ansi bv i len)))
        (cond (end (values end j))
              ((< (+ i 1) len)
               (let ((b (bytevector-u8-ref bv (+ i 1))))
                 (if
                   ;; 合法 Fe/Fs/Fp/nF 终结字节：连同 ESC 一并消费。
                   ;; 其他字母只删除裸 ESC，保留普通正文（例如 ESC b）。
                   (or (and (>= b 48) (<= b 63))
                     (and (>= b 32) (<= b 47))
                     (and (>= b 65) (<= b 80))
                     (memv b
                       '(82 83 84 90 99 102 103 104 105 106 107 108 109 110 113
                         114 115 116 117 121)
                     ) ;memv
                   ) ;or
                   (values (+ i 2) j)
                   (values (+ i 1) j)
                 ) ;if
               ) ;let
              ) ;
              (else (values (+ i 1) j))
        ) ;cond
      ) ;let
    ) ;define

    (define (%sanitize-binary-output str)
      (let* ((bv (string->byte-vector str))
             (len (bytevector-length bv))
             (keep (make-bytevector len 0))
            ) ;
        (let loop
          ((i 0) (j 0))
          (if (>= i len)
            (utf8-replace-invalid (byte-vector->string (if (= j len) keep (bytevector-copy keep 0 j)))
            ) ;utf8-replace-invalid
            (let ((b (bytevector-u8-ref bv i)))
              (cond
               ((= b 27)
                (let-values (((next next-j) (%copy-safe-escape-sequence! bv i len keep j)))
                  (loop next next-j)
                ) ;let-values
               ) ;
               ;; DEL、C0（保留 \t \n \r）
               ((or (= b 127) (and (< b 32) (not (= b 9)) (not (= b 10)) (not (= b 13))))
                (loop (+ i 1) j)
               ) ;
               ;; C1：合法 UTF-8 为 C2 80..9F。控制序列先整体剥离，
               ;; 其余控制字符删除，不能让其后的文本被当成序列载荷。
               ((and (= b 194)
                  (< (+ i 1) len)
                  (>= (bytevector-u8-ref bv (+ i 1)) 128)
                  (<= (bytevector-u8-ref bv (+ i 1)) 159)
                ) ;and
                (let ((end (%match-ansi bv i len)))
                  (if end (loop end j) (loop (+ i 2) j))
                ) ;let
               ) ;
               ;; U+FFF9..U+FFFB：interlinear annotation
               ((and (< (+ i 2) len)
                  (= b 239)
                  (= (bytevector-u8-ref bv (+ i 1)) 191)
                  (let ((c (bytevector-u8-ref bv (+ i 2))))
                    (and (>= c 185) (<= c 187))
                  ) ;let
                ) ;and
                (loop (+ i 3) j)
               ) ;
               (else (bytevector-u8-set! keep j b) (loop (+ i 1) (+ j 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (sanitize-binary-output str)
      (unless (string? str)
        (type-error "sanitize-binary-output: str must be a string")
      ) ;unless
      (let* ((bv (string->byte-vector str)) (len (bytevector-length bv)))
        (if (%bv-needs-clean? bv len)
          (%sanitize-binary-output str)
          (utf8-replace-invalid str)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
