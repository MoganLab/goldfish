;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui text-width core)
  (export %match-ansi-csi %match-ansi-osc %match-ansi %codepoint-width
    %is-sgr-reset? strip-ansi display-width display-width-prefix
  ) ;export
  (import (scheme base) (liii base) (liii error) (liii unicode))
  (begin

    (define (%match-ansi-csi bv i len)
      (if
        (and (< (+ i 1) len)
          (= (bytevector-u8-ref bv i) 27)
          (= (bytevector-u8-ref bv (+ i 1)) 91)
        ) ;and
        (let loop
          ((j (+ i 2)))
          (if (>= j len)
            #f
            (let ((b (bytevector-u8-ref bv j)))
              (cond ((and (>= b 32) (<= b 63)) (loop (+ j 1)))
                    ((and (>= b 64) (<= b 126)) (+ j 1))
                    (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; OSC 序列：ESC ] ... ST，ST ∈ { BEL(0x07)，ESC \(27 92)，C1 ST(0x9C) }
    ;; 对齐上游 ansi-regex：OSC 到第一个终结符为止。
    ;; 无终结符（不完整 OSC）返回 #f：保守不剥，等待流式后续块补全
    (define (%match-ansi-osc bv i len)
      (if
        (and (< (+ i 1) len)
          (= (bytevector-u8-ref bv i) 27)
          (= (bytevector-u8-ref bv (+ i 1)) 93)
        ) ;and
        (let loop
          ((j (+ i 2)))
          (if (>= j len)
            #f
            (let ((b (bytevector-u8-ref bv j)))
              (cond ((= b 7) (+ j 1))
                    ((and (= b 27) (< (+ j 1) len) (= (bytevector-u8-ref bv (+ j 1)) 92)) (+ j 2))
                    ((= b 156) (+ j 1))
                    (else (loop (+ j 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; 任意 ANSI 转义序列（CSI 或 OSC）的结束位置，均不匹配时返回 #f
    (define (%match-ansi bv i len)
      (or (%match-ansi-csi bv i len) (%match-ansi-osc bv i len))
    ) ;define

    (define (%codepoint-width cp)
      (cond
       ((or (< cp 32) (and (>= cp 127) (< cp 160))) 0)
       ((or (= cp 173)
          (= cp 8203)
          (= cp 8204)
          (= cp 8205)
          (= cp 65279)
          (and (>= cp 768) (<= cp 879))
          (and (>= cp 7616) (<= cp 7679))
          (and (>= cp 8400) (<= cp 8447))
          (and (>= cp 65056) (<= cp 65071))
        ) ;or
        0
       ) ;
       ((or (and (>= cp 4352) (<= cp 4447))
          (and (>= cp 11904) (<= cp 42191))
          (and (>= cp 44032) (<= cp 55203))
          (and (>= cp 63744) (<= cp 64255))
          (and (>= cp 65040) (<= cp 65049))
          (and (>= cp 65072) (<= cp 65135))
          (and (>= cp 65280) (<= cp 65376))
          (and (>= cp 65504) (<= cp 65510))
          (and (>= cp 131072) (<= cp 196605))
          (and (>= cp 196608) (<= cp 262141))
          (and (>= cp 9728) (<= cp 10175))
          (and (>= cp 127744) (<= cp 129791))
        ) ;or
        2
       ) ;
       (else 1)
      ) ;cond
    ) ;define

    (define (%is-sgr-reset? s)
      (or (string=? s "\x1b;[0m") (string=? s "\x1b;[m"))
    ) ;define

    ;; 快速路径：不含 ESC 字节时无转义序列可剥，直接返回原串
    ;; （对齐上游 ansi-regex 的 includes("\\u001B") 检查，
    ;; 避免渲染层每帧对纯文本输出做逐字节重建的浪费）
    (define (%bv-contains-esc? bv len)
      (let loop
        ((i 0))
        (if (>= i len) #f (if (= (bytevector-u8-ref bv i) 27) #t (loop (+ i 1))))
      ) ;let
    ) ;define

    ;; 剔除 ANSI 转义序列；同时把非法 UTF-8 字节替换为 U+FFFD，
    ;; 避免 utf8->string 对非法字节抛 value-error（子进程输出可能含
    ;; 被截断的多字节序列或二进制垃圾）
    (define (strip-ansi str)
      (unless (string? str)
        (type-error "strip-ansi: str must be a string")
      ) ;unless
      (if (string=? str "")
        ""
        (let* ((bv (string->byte-vector str)) (len (bytevector-length bv)))
          (if (not (%bv-contains-esc? bv len))
            (utf8-replace-invalid str)
            (let loop
              ((i 0) (acc '()))
              (if (>= i len)
                (utf8-replace-invalid (apply string-append (reverse acc)))
                (let ((ansi-end (%match-ansi bv i len)))
                  (if ansi-end
                    (loop ansi-end acc)
                    (let* ((next (bytevector-advance-utf8 bv i len))
                           (step (if (= next i) (+ i 1) next))
                           ;; 非法字节用原始字节保留，最后统一替换为 U+FFFD
                           (chunk (byte-vector->string (bytevector-copy bv i step)))
                          ) ;
                      (loop step (cons chunk acc))
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (display-width str)
      (unless (string? str)
        (type-error "display-width: str must be a string")
      ) ;unless
      (if (string=? str "")
        0
        (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (w 0))
            (if (>= i len)
              w
              (let ((ansi-end (%match-ansi bv i len)))
                (if ansi-end
                  (loop ansi-end w)
                  (let* ((next (bytevector-advance-utf8 bv i len))
                         (step (if (= next i) (+ i 1) next))
                         (cp (utf8->codepoint-at bv i))
                         (cw (%codepoint-width cp))
                        ) ;
                    (loop step (+ w cw))
                  ) ;let*
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    ;; ; display-width-prefix: 前 n 个 Unicode 字符占用的终端列宽
    ;;
    ;; n 是字符（码点）个数，不是字节数；宽度规则与 display-width 完全一致。
    (define (display-width-prefix str n)
      (unless (string? str)
        (type-error "display-width-prefix: str must be a string")
      ) ;unless
      (unless (and (integer? n) (>= n 0))
        (type-error "display-width-prefix: n must be a non-negative integer")
      ) ;unless
      (if (or (= n 0) (string=? str ""))
        0
        (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (count 0) (w 0))
            (if (or (>= count n) (>= i len))
              w
              (let ((ansi-end (%match-ansi bv i len)))
                (if ansi-end
                  (loop ansi-end count w)
                  (let* ((next (bytevector-advance-utf8 bv i len))
                         (step (if (= next i) (+ i 1) next))
                         (cp (utf8->codepoint-at bv i))
                         (cw (%codepoint-width cp))
                        ) ;
                    (loop step (+ count 1) (+ w cw))
                  ) ;let*
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
