;;
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

;; SGR 选择性复位（issue #1484）：
;; 折行收尾只复位行内仍生效的 SGR 样式（如前景 \x1b[39m、粗体 \x1b[22m），
;; 绝不发 \x1b[0m 全重置，避免清掉调用方在行外设置的背景等样式。
(define-library (liii tui text-width sgr)
  (export %sgr-selective-reset)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii tui text-width core)
  ) ;import
  (begin

    ;; 样式类别按 ECMA-48 SGR 参数归类；未收录的参数不跟踪、不复位
    (define %sgr-on-classes
      '((1 . bold)
        (2 . faint)
        (3 . italic)
        (4 . underline)
        (5 . blink)
        (6 . blink)
        (7 . inverse)
        (8 . invisible)
        (9 . strike))
    ) ;define

    (define %sgr-off-map
      '((22 bold faint)
        (23 italic)
        (24 underline)
        (25 blink)
        (27 inverse)
        (28 invisible)
        (29 strike))
    ) ;define

    ;; 样式类别 → 选择性复位参数（bold/faint 共用 22，去重后输出）
    (define %sgr-reset-codes
      '((fg . 39)
        (bg . 49)
        (bold . 22)
        (faint . 22)
        (italic . 23)
        (underline . 24)
        (blink . 25)
        (inverse . 27)
        (invisible . 28)
        (strike . 29))
    ) ;define

    (define (%sgr-adjoin cls state)
      (if (memq cls state) state (cons cls state))
    ) ;define

    (define (%sgr-remove classes state)
      (filter (lambda (c) (not (memq c classes))) state)
    ) ;define

    (define (%sgr-set-class p)
      (cond ((assv p %sgr-on-classes) => cdr)
            ((or (and (>= p 30) (<= p 37)) (and (>= p 90) (<= p 97))) 'fg)
            ((or (and (>= p 40) (<= p 47)) (and (>= p 100) (<= p 107))) 'bg)
            (else #f)
      ) ;cond
    ) ;define

    ;; ps 以 38/48 开头：扩展色 38;5;n 消费 3 个参数、38;2;r;g;b 消费 5 个，
    ;; 参数不足时按实际长度消费，返回消费个数
    (define (%sgr-extended-consumed ps)
      (if (null? (cdr ps))
        1
        (let ((mode (cadr ps)))
          (cond ((= mode 5) (min (length ps) 3))
                ((= mode 2) (min (length ps) 5))
                (else 1)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 消费一个 SGR 参数（可能连带扩展色参数），返回 (新状态 . 消费个数)
    (define (%sgr-step ps state)
      (let ((p (car ps)))
        (cond ((= p 0) (cons '() 1))
              ((= p 39) (cons (%sgr-remove '(fg) state) 1))
              ((= p 49) (cons (%sgr-remove '(bg) state) 1))
              ((assv p %sgr-off-map) => (lambda (e) (cons (%sgr-remove (cdr e) state) 1)))
              ((or (= p 38) (= p 48))
               (let ((n (%sgr-extended-consumed ps)))
                 (cons (%sgr-adjoin (if (= p 38) 'fg 'bg) state) n)
               ) ;let
              ) ;
              ((%sgr-set-class p) => (lambda (cls) (cons (%sgr-adjoin cls state) 1)))
              (else (cons state 1))
        ) ;cond
      ) ;let
    ) ;define

    (define (%sgr-fold-params params state)
      (let loop
        ((ps params) (st state))
        (if (null? ps)
          st
          (let ((step (%sgr-step ps st)))
            (loop (list-tail ps (cdr step)) (car step))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; ESC [ ... m 的参数数字列表（空参数按 0 处理）
    (define (%sgr-params-at bv from to)
      (map (lambda (part) (or (string->number part) 0))
        (string-split (utf8->string (bytevector-copy bv from to)) ";")
      ) ;map
    ) ;define

    ;; 扫描一串转义序列，返回作用后的样式类别集合；非 SGR 序列忽略
    (define (%sgr-scan bv i len state)
      (if (>= i len)
        state
        (let ((end (%match-ansi-csi bv i len)))
          (cond ((not end) (%sgr-scan bv (+ i 1) len state))
                ((not (= (bytevector-u8-ref bv (- end 1)) 109)) (%sgr-scan bv end len state))
                (else
                  (let ((params (%sgr-params-at bv (+ i 2) (- end 1))))
                    (%sgr-scan bv end len (%sgr-fold-params params state))
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 复位参数按升序插入去重
    (define (%sgr-insert-code code codes)
      (cond ((null? codes) (list code))
            ((= code (car codes)) codes)
            ((< code (car codes)) (cons code codes))
            (else (cons (car codes) (%sgr-insert-code code (cdr codes))))
      ) ;cond
    ) ;define

    (define (%sgr-reset-string state)
      (let loop
        ((st state) (codes '()))
        (if (null? st)
          (if (null? codes)
            ""
            (string-append "\x1b;[" (string-join (map number->string codes) ";") "m")
          ) ;if
          (let ((e (assv (car st) %sgr-reset-codes)))
            (loop (cdr st) (if e (%sgr-insert-code (cdr e) codes) codes))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 行尾选择性复位串：一串生效中的 SGR 序列 → 只复位其实际生效样式的
    ;; 一条 SGR（参数升序合并）；无生效样式返回 ""
    (define (%sgr-selective-reset active-ansi)
      (if (string=? active-ansi "")
        ""
        (let* ((bv (string->byte-vector active-ansi)) (len (bytevector-length bv)))
          (%sgr-reset-string (%sgr-scan bv 0 len '()))
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
