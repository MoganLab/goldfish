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

(define-library (liii tui terminal poll)
  (export terminal-poll-key %poll-console-key %resolve-ansi-bytes
    %resolve-utf8-bytes %map-extended-key %map-standard-key
  ) ;export
  (import (scheme base)
    (liii error)
    (liii os)
    (liii unicode)
    (liii tui terminal protocol)
    (liii tui terminal virtual)
    (liii tui keys parser)
  ) ;import
  (begin

    (define (%map-extended-key code)
      (cond ((= code 72) 'up)
            ((= code 80) 'down)
            ((= code 75) 'left)
            ((= code 77) 'right)
            ((= code 71) 'home)
            ((= code 79) 'end)
            ((= code 73) 'page-up)
            ((= code 81) 'page-down)
            ((= code 83) 'delete)
            ((= code 82) 'insert)
            ((or (= code 15) (= code 353)) 'shift-tab)
            ((= code 115) 'ctrl-left)
            ((= code 116) 'ctrl-right)
            ((= code 119) 'ctrl-home)
            ((= code 117) 'ctrl-end)
            ((= code 141) 'ctrl-up)
            ((= code 145) 'ctrl-down)
            ((= code 147) 'ctrl-delete)
            ((and (>= code 59) (<= code 68))
             (string->symbol (string-append "f" (number->string (- code 58))))
            ) ;
            ((= code 133) 'f11)
            ((= code 134) 'f12)
            (else #f)
      ) ;cond
    ) ;define

    (define (%map-standard-key code)
      (cond ((= code 13) 'enter)
            ((= code 10) 'ctrl-j)
            ((= code 27) 'escape)
            ((or (= code 8) (= code 127)) 'backspace)
            ((= code 9) 'tab)
            ;; 裸字节 15 是真正的 Ctrl+O（pty/Windows 控制台层均如此）；
            ;; Shift+Tab 由 C++ 控制台层编码为 353（curses KEY_BTAB），
            ;; DOS conio 的 15 扩展码仅出现在 0/224 前缀之后（%map-extended-key）
            ((= code 353) 'shift-tab)
            ((= code 3) 'ctrl-c)
            ((= code 4) 'ctrl-d)
            ((and (>= code 32) (<= code 126)) (string (integer->char code)))
            (else (parse-key (string (integer->char code))))
      ) ;cond
    ) ;define

    (define (%resolve-ansi-bytes byte-list)
      (if (null? byte-list)
        #f
        (if (<= (length byte-list) 1)
          'escape
          (let* ((str (list->string (map integer->char byte-list))) (k (parse-key str)))
            k
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (%resolve-utf8-bytes byte-list)
      (if (null? byte-list)
        #f
        (catch #t
          (lambda () (utf8->string (apply bytevector byte-list)))
          (lambda args #f)
        ) ;catch
      ) ;if
    ) ;define

    (define (%read-continuation-byte)
      (if (console-kbhit? 20)
        (let ((b (console-getch)))
          (if (and b (exact-integer? b) (>= b 128) (<= b 191)) b #f)
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; ; 读取一个 UTF-8 多字节字符：首字节已读入，还需读取后续的 total-len-1 个连续字节
    (define (%poll-utf8-char first-byte total-len)
      (let loop
        ((count (- total-len 1)) (acc (list first-byte)))
        (if (= count 0)
          (%resolve-utf8-bytes (reverse acc))
          (let ((b (%read-continuation-byte)))
            (if b (loop (- count 1) (cons b acc)) #f)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%poll-escape-key)
      (if (not (console-kbhit? 25))
        'escape
        (let loop
          ((bytes (list 27)))
          (if (console-kbhit? 10)
            (let ((b (console-getch)))
              (if (and b (exact-integer? b))
                (loop (cons b bytes))
                (%resolve-ansi-bytes (reverse bytes))
              ) ;if
            ) ;let
            (%resolve-ansi-bytes (reverse bytes))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%poll-console-key)
      (if (console-kbhit?)
        (let ((c (console-getch)))
          (cond
           ((not (and c (exact-integer? c))) #f)
           ((and (os-windows?) (or (= c 0) (= c 224)))
            (let ((ext (console-getch)))
              (if (and ext (exact-integer? ext)) (%map-extended-key ext) #f)
            ) ;let
           ) ;
           ((= c 27) (%poll-escape-key))
           ((>= c 194)
            (let ((seq-len (utf8-sequence-length c)))
              (if seq-len (%poll-utf8-char c seq-len) (%map-standard-key c))
            ) ;let
           ) ;
           (else (%map-standard-key c))
          ) ;cond
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (terminal-poll-key . args)
      (let ((term (if (null? args) #f (car args))))
        (cond ((not term) (%poll-console-key))
              ((virtual-terminal? term) (virtual-terminal-pop-key! term))
              ((terminal? term) (%poll-console-key))
              (else (type-error "terminal-poll-key: expected a terminal or #f"))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
