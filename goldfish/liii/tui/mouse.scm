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

;; SGR 鼠标事件解析（XTerm 1006 模式）。
;; 参考 ~/git/pi 的 tui-alt-screen.ts：序列形如 ESC[<B;X;YM（按下）/ m（释放），
;; B 为按钮码：低 2 位 0/1/2=左/中/右，bit3(8)=Alt，bit4(4)=Shift，bit5(16)=Ctrl，
;; bit6(32)=motion（拖动），bit7(64)=滚轮。X/Y 为 1-based 屏幕坐标。
;; Windows 控制台层（src/liii_os.cpp）将 MOUSE_EVENT 合成为同样的 SGR
;; 字节序列，因此 Scheme 侧只需这一条解析路径。

(define-library (liii tui mouse)
  (export make-mouse-event mouse-event? mouse-event-type mouse-event-button
    mouse-event-x mouse-event-y mouse-event-shift? mouse-event-ctrl?
    mouse-event-alt? mouse-event-click? parse-mouse-event mouse-sequence?
  ) ;export
  (import (scheme base) (liii base) (liii error) (liii string))
  (begin

    (define-record-type <mouse-event>
      (%make-mouse-event type button x y shift? ctrl? alt?)
      mouse-event?
      (type mouse-event-type)
      (button mouse-event-button)
      (x mouse-event-x)
      (y mouse-event-y)
      (shift? mouse-event-shift?)
      (ctrl? mouse-event-ctrl?)
      (alt? mouse-event-alt?)
    ) ;define-record-type

    (define (make-mouse-event type button x y . opt)
      (let ((shift? (and (pair? opt) (car opt)))
            (ctrl? (and (pair? opt) (pair? (cdr opt)) (cadr opt)))
            (alt? (and (pair? opt) (pair? (cdr opt)) (pair? (cddr opt)) (caddr opt)))
           ) ;
        (%make-mouse-event type button x y (and shift? #t) (and ctrl? #t) (and alt? #t))
      ) ;let
    ) ;define

    ;; 按钮码低 2 位 -> 符号；35（无按钮 motion）归为 none
    (define (%decode-button b)
      (case (modulo b 4)
            ((0) 'left)
            ((1) 'middle)
            ((2) 'right)
            (else 'none)
      ) ;case
    ) ;define

    ;; B;X;Y -> (list bstr xstr ystr)，形态不符返回 #f
    (define (%split-mouse-params str)
      (let ((parts (string-split str ";")))
        (if (= (length parts) 3)
          (let ((b (string->number (car parts)))
                (x (string->number (cadr parts)))
                (y (string->number (caddr parts)))
               ) ;
            (if (and b x y) (list b x y) #f)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; 事件类型：滚轮优先，其次释放、拖动、按下
    (define (%event-type b release?)
      (cond
       ((>= b 64) (if (= (modulo b 4) 0) 'wheel-up 'wheel-down))
       (release? 'release)
       ((>= (modulo b 128) 32) 'drag)
       (else 'press)
      ) ;cond
    ) ;define

    (define (%event-button b release?)
      (cond ((>= b 64) 'none)
            ((and release? (= (modulo b 4) 3)) 'none)
            (else (%decode-button b))
      ) ;cond
    ) ;define

    ;; 解析 "ESC[<B;X;YM" / "ESC[<B;X;Ym"，坐标转为 0-based
    (define (parse-mouse-event str)
      (unless (string? str)
        (type-error "parse-mouse-event: str must be a string")
      ) ;unless
      (let ((len (string-length str)))
        (if
          (and (>= len 9)
            (char=? (string-ref str 0) #\x1b)
            (char=? (string-ref str 1) #\[)
            (char=? (string-ref str 2) #\<)
            (or (char=? (string-ref str (- len 1)) #\M)
              (char=? (string-ref str (- len 1)) #\m)
            ) ;or
          ) ;and
          (let ((params (%split-mouse-params (substring str 3 (- len 1)))))
            (if params
              (let* ((b (car params))
                     (x (cadr params))
                     (y (caddr params))
                     (release? (char=? (string-ref str (- len 1)) #\m))
                    ) ;
                (%make-mouse-event (%event-type b release?)
                  (%event-button b release?)
                  (- x 1)
                  (- y 1)
                  (= (modulo (quotient b 4) 2) 1)
                  (>= (modulo b 32) 16)
                  (>= (modulo b 16) 8)
                ) ;%make-mouse-event
              ) ;let*
              #f
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; 判断字节串内容是否为完整 SGR 鼠标序列（用于 poll 层分流）
    (define (mouse-sequence? str)
      (and (parse-mouse-event str) #t)
    ) ;define

    ;; click 是 press 与同位置同按钮 release 的合成事件（由 poll 层产生），
    ;; 对齐上游 pi tui-alt-screen.ts 的 click 合成机制
    (define (mouse-event-click? ev)
      (unless (mouse-event? ev)
        (type-error "mouse-event-click?: ev must be a mouse-event")
      ) ;unless
      (eq? (mouse-event-type ev) 'click)
    ) ;define

  ) ;begin
) ;define-library
