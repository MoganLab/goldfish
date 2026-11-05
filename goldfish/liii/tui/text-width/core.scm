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

    ;; ESC 引导的双字节序列：覆盖 ECMA-48 的 Fe/Fs/Fp/nF 终结字节。
    ;; 包括 RIS（ESC c）、保存/恢复光标（ESC 7/8）、键盘模式（ESC =/>）
    ;; 及字符串终结符（ESC \）。终结字节本身也必须消费，不能只删除 ESC。
    ;; 普通字母（如 ESC b）不是合法 Fe 序列，不能误吞后续文本。
    (define (%match-ansi-esc bv i len)
      (if
        (and (< (+ i 1) len)
          (= (bytevector-u8-ref bv i) 27)
          (let ((b (bytevector-u8-ref bv (+ i 1))))
            (or (and (>= b 48) (<= b 63))
              (and (>= b 32) (<= b 47) (< (+ i 2) len) (>= (bytevector-u8-ref bv (+ i 2)) 48))
              ;; [ ] P ^ _ X 是 CSI 或字符串序列的引导符，由前面的
              ;; 专用匹配器处理；不完整时不能当作独立短 ESC 序列。
              (and (>= b 64) (<= b 95) (not (memv b '(80 88 91 93 94 95))))
              (memv b '(99 102 103 104 105 106 107 108 109 110 113 114 115 116
                        117 121))
            ) ;or
          ) ;let
        ) ;and
        (let ((b (bytevector-u8-ref bv (+ i 1))))
          (if (and (>= b 32) (<= b 47)) (min len (+ i 3)) (+ i 2))
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; DCS/SOS/PM/APC 字符串序列：与 OSC 一样等待 BEL、ESC \ 或 C1 ST。
    (define (%match-ansi-string bv i len)
      (if
        (and (< (+ i 1) len)
          (= (bytevector-u8-ref bv i) 27)
          (memv (bytevector-u8-ref bv (+ i 1)) '(80 88 94 95))
        ) ;and
        (let loop
          ((j (+ i 2)))
          (if (>= j len)
            #f
            (let ((b (bytevector-u8-ref bv j)))
              (cond ((= b 7) (+ j 1))
                    ((and (= b 27) (< (+ j 1) len) (= (bytevector-u8-ref bv (+ j 1)) 92)) (+ j 2))
                    ((= b 156) (+ j 1))
                    ((and (= b 194) (< (+ j 1) len) (= (bytevector-u8-ref bv (+ j 1)) 156)) (+ j 2))
                    (else (loop (+ j 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; 单字节 C1 CSI（U+009B，UTF-8 C2 9B）。
    (define (%match-ansi-c1-csi bv i len)
      (if
        (and (< (+ i 1) len)
          (= (bytevector-u8-ref bv i) 194)
          (= (bytevector-u8-ref bv (+ i 1)) 155)
        ) ;and
        (let loop
          ((j (+ i 2)) (prefix? #t))
          (if (>= j len)
            #f
            (let ((b (bytevector-u8-ref bv j)))
              (cond
               ((and prefix? (memv b '(40 41 91 93))) (loop (+ j 1) #t))
               ((and (>= b 48) (<= b 63)) (loop (+ j 1) #f))
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
                    ((and (= b 194) (< (+ j 1) len) (= (bytevector-u8-ref bv (+ j 1)) 156)) (+ j 2))
                    (else (loop (+ j 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; 任意 ANSI 转义序列（CSI、OSC 或短 ESC 序列）的结束位置，
    ;; 均不匹配时返回 #f。CSI/OSC 优先于短 ESC，保证其载荷不被逐字节输出。
    (define (%match-ansi bv i len)
      (or (%match-ansi-csi bv i len)
        (%match-ansi-osc bv i len)
        (%match-ansi-string bv i len)
        (%match-ansi-c1-csi bv i len)
        (%match-ansi-esc bv i len)
      ) ;or
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

    ;; 从 start 开始查找下一个 ESC(27) 字节的位置，找不到返回 #f
    (define (%bv-next-esc bv start len)
      (let loop
        ((i start))
        (if (>= i len) #f (if (= (bytevector-u8-ref bv i) 27) i (loop (+ i 1))))
      ) ;let
    ) ;define

    (define (%bv-next-c1-csi bv start len)
      (let loop
        ((i start))
        (cond ((>= (+ i 1) len) #f)
              ((and (= (bytevector-u8-ref bv i) 194) (= (bytevector-u8-ref bv (+ i 1)) 155))
               i
              ) ;
              (else (loop (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%bv-next-ansi-start bv start len)
      (let ((esc (%bv-next-esc bv start len)) (c1 (%bv-next-c1-csi bv start len)))
        (if (and esc c1) (min esc c1) (or esc c1 len))
      ) ;let
    ) ;define

    (define (%strip-ansi-loop bv len i acc)
      (if (>= i len)
        (apply string-append (reverse acc))
        (let ((ansi-end (%match-ansi bv i len)))
          (if ansi-end
            (%strip-ansi-loop bv len ansi-end acc)
            (let* ((next (%bv-next-ansi-start bv (+ i 1) len)) (chunk (%clean-segment bv i next)))
              (%strip-ansi-loop bv len next (cons chunk acc))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 段内文本：整段复制并把非法 UTF-8 字节替换为 U+FFFD
    (define (%clean-segment bv start end)
      (utf8-replace-invalid (byte-vector->string (bytevector-copy bv start end)))
    ) ;define

    ;; 剔除 ANSI 转义序列；同时把非法 UTF-8 字节替换为 U+FFFD，
    ;; 避免 utf8->string 对非法字节抛 value-error（子进程输出可能含
    ;; 被截断的多字节序列或二进制垃圾）。
    ;; 单遍扫描：ANSI 序列按段剥除，段间文本整段做一次 utf8-replace-invalid
    (define (strip-ansi str)
      (unless (string? str)
        (type-error "strip-ansi: str must be a string")
      ) ;unless
      (if (string=? str "")
        ""
        (let* ((bv (string->byte-vector str)) (len (bytevector-length bv)))
          (if (not (%bv-contains-esc? bv len))
            (utf8-replace-invalid str)
            (%strip-ansi-loop bv len 0 '())
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
        (let* ((bv (string->byte-vector str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (w 0))
            (if (>= i len)
              w
              (let ((ansi-end (%match-ansi bv i len)))
                (if ansi-end
                  (loop ansi-end w)
                  (let ((next (bytevector-advance-utf8-strict bv i len)))
                    (if (= next i)
                      ;; 非法 UTF-8 字节：对替换为 U+FFFD 后的串重算宽度，
                      ;; 与清理后文本的实际渲染宽度一致（U+FFFD 宽 1）；
                      ;; 替换结果为严格合法 UTF-8，重入后不会再触发本分支
                      (display-width (utf8-replace-invalid str))
                      (let* ((cp (utf8->codepoint-at bv i)) (cw (%codepoint-width cp)))
                        (loop next (+ w cw))
                      ) ;let*
                    ) ;if
                  ) ;let
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
        (let* ((bv (string->byte-vector str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (count 0) (w 0))
            (if (or (>= count n) (>= i len))
              w
              (let ((ansi-end (%match-ansi bv i len)))
                (if ansi-end
                  (loop ansi-end count w)
                  (let ((next (bytevector-advance-utf8-strict bv i len)))
                    (if (= next i)
                      ;; 非法 UTF-8 字节：对替换为 U+FFFD 后的串重算（同 display-width）
                      (display-width-prefix (utf8-replace-invalid str) n)
                      (let* ((cp (utf8->codepoint-at bv i)) (cw (%codepoint-width cp)))
                        (loop next (+ count 1) (+ w cw))
                      ) ;let*
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
