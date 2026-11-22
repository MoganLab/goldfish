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

(define-library (liii tui keys table)
  (export %ctrl-char-key %ss3-key %csi-final-key %tilde-key %linux-fn-key
    %modifier-prefix %csi-final-byte? %csi-meta-prefix? %meta-escape-prefix?
    %meta-escape-complete? %alt-wrap
  ) ;export
  (import (scheme base))
  (begin

    (define (%ctrl-char-key byte)
      (cond ((= byte 0) 'ctrl-space)
            ((= byte 1) 'ctrl-a)
            ((= byte 2) 'ctrl-b)
            ((= byte 3) 'ctrl-c)
            ((= byte 4) 'ctrl-d)
            ((= byte 5) 'ctrl-e)
            ((= byte 6) 'ctrl-f)
            ((= byte 7) 'ctrl-g)
            ((= byte 8) 'backspace)
            ((= byte 9) 'tab)
            ((= byte 10) 'ctrl-j)
            ((= byte 11) 'ctrl-k)
            ((= byte 12) 'ctrl-l)
            ((= byte 13) 'enter)
            ((= byte 14) 'ctrl-n)
            ((= byte 15) 'ctrl-o)
            ((= byte 16) 'ctrl-p)
            ((= byte 17) 'ctrl-q)
            ((= byte 18) 'ctrl-r)
            ((= byte 19) 'ctrl-s)
            ((= byte 20) 'ctrl-t)
            ((= byte 21) 'ctrl-u)
            ((= byte 22) 'ctrl-v)
            ((= byte 23) 'ctrl-w)
            ((= byte 24) 'ctrl-x)
            ((= byte 25) 'ctrl-y)
            ((= byte 26) 'ctrl-z)
            ((= byte 27) 'escape)
            ((= byte 28) 'ctrl-\)
            ((= byte 29) 'ctrl-])
            ((= byte 30) 'ctrl-^)
            ((= byte 31) 'ctrl-_)
            ((= byte 127) 'backspace)
            (else #f)
      ) ;cond
    ) ;define

    (define (%ss3-key char-byte)
      (cond ((= char-byte 65) 'up)
            ((= char-byte 66) 'down)
            ((= char-byte 67) 'right)
            ((= char-byte 68) 'left)
            ((= char-byte 69) 'clear)
            ((= char-byte 70) 'end)
            ((= char-byte 72) 'home)
            ((= char-byte 77) 'shift-enter)
            ((= char-byte 80) 'f1)
            ((= char-byte 81) 'f2)
            ((= char-byte 82) 'f3)
            ((= char-byte 83) 'f4)
            (else #f)
      ) ;cond
    ) ;define

    (define (%csi-final-key char-byte)
      (cond ((= char-byte 65) 'up)
            ((= char-byte 66) 'down)
            ((= char-byte 67) 'right)
            ((= char-byte 68) 'left)
            ((= char-byte 69) 'clear)
            ((= char-byte 70) 'end)
            ((= char-byte 72) 'home)
            ((= char-byte 90) 'shift-tab)
            (else #f)
      ) ;cond
    ) ;define

    (define (%tilde-key num-str)
      (cond ((string=? num-str "1") 'home)
            ((string=? num-str "2") 'insert)
            ((string=? num-str "3") 'delete)
            ((string=? num-str "4") 'end)
            ((string=? num-str "5") 'page-up)
            ((string=? num-str "6") 'page-down)
            ((string=? num-str "7") 'home)
            ((string=? num-str "8") 'end)
            ((string=? num-str "11") 'f1)
            ((string=? num-str "12") 'f2)
            ((string=? num-str "13") 'f3)
            ((string=? num-str "14") 'f4)
            ((string=? num-str "15") 'f5)
            ((string=? num-str "17") 'f6)
            ((string=? num-str "18") 'f7)
            ((string=? num-str "19") 'f8)
            ((string=? num-str "20") 'f9)
            ((string=? num-str "21") 'f10)
            ((string=? num-str "23") 'f11)
            ((string=? num-str "24") 'f12)
            (else #f)
      ) ;cond
    ) ;define

    (define (%linux-fn-key char-byte)
      (cond ((= char-byte 65) 'f1)
            ((= char-byte 66) 'f2)
            ((= char-byte 67) 'f3)
            ((= char-byte 68) 'f4)
            ((= char-byte 69) 'f5)
            (else #f)
      ) ;cond
    ) ;define

    (define (%modifier-prefix mod-code)
      (cond ((= mod-code 2) "shift-")
            ((= mod-code 3) "alt-")
            ((= mod-code 4) "shift-alt-")
            ((= mod-code 5) "ctrl-")
            ((= mod-code 6) "shift-ctrl-")
            ((= mod-code 7) "alt-ctrl-")
            ((= mod-code 8) "shift-alt-ctrl-")
            (else "")
      ) ;cond
    ) ;define

    ;; CSI 终结字节（0x40~0x7E）：序列到此结束
    (define (%csi-final-byte? b)
      (and (integer? b) (>= b 64) (<= b 126))
    ) ;define

    ;; 键名是否已带 alt- 前缀（裸 "alt-" 不算）
    (define (%alt-key-name? s)
      (and (> (string-length s) 4) (string=? (substring s 0 4) "alt-"))
    ) ;define

    ;; 双 ESC Meta 序列中第二个 ESC 之后允许的序列起始字节：
    ;; [ (91) 开 CSI、O (79) 开 SS3
    (define (%csi-meta-prefix? byte)
      (or (= byte 91) (= byte 79))
    ) ;define

    ;; bytes（按到达顺序）是否为双 ESC Meta 序列前缀：macOS 终端把
    ;; Option 键配置为 Meta/Esc+ 时，Option+方向键等发送 ESC ESC [ A
    ;; 之类序列，第二个 ESC 是 Meta 前缀而非独立按键；
    ;; 含仅 ESC ESC 两字节的待定形态（第三字节未到达时无法排除）
    (define (%meta-escape-prefix? bytes)
      (and (pair? (cdr bytes))
        (= (car bytes) 27)
        (= (cadr bytes) 27)
        (or (null? (cddr bytes)) (%csi-meta-prefix? (car (cddr bytes))))
      ) ;and
    ) ;define

    ;; 双 ESC Meta 序列是否已聚齐：与 CSI 序列同用终结字节判定，
    ;; ESC ESC [ ... <终结字节> 与 ESC ESC O <键> 均至少 4 字节
    (define (%meta-escape-complete? bytes)
      (and (> (length bytes) 3)
        (%csi-final-byte? (list-ref bytes (- (length bytes) 1)))
      ) ;and
    ) ;define

    ;; Meta 前缀与基础键合成 alt- 组合键：'escape、#f 与非符号键
    ;; 原样返回；键名已带 alt- 前缀则原样保留（重复 Meta 合并，
    ;; 如 ESC ESC [1;3A → alt-up）；其余拼 alt- 前缀
    ;; （如 ESC ESC [1;5A → alt-ctrl-up）
    (define (%alt-wrap k)
      (let ((s (and (symbol? k) (symbol->string k))))
        (cond ((not s) k)
              ((or (string=? s "escape") (%alt-key-name? s)) k)
              (else (string->symbol (string-append "alt-" s)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
