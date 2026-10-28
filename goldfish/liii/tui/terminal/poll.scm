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
  (export terminal-poll-key terminal-paste-mode? terminal-set-paste-mode!
    %poll-console-key %resolve-ansi-bytes %resolve-utf8-bytes %map-extended-key
    %map-standard-key
  ) ;export
    (import (scheme base)
    (liii error)
    (liii list)
    (liii os)
    (liii string)
    (liii unicode)
    (liii tui terminal protocol)
    (liii tui terminal virtual)
    (liii tui keys parser)
    (liii tui mouse)
  ) ;import
  (begin

    ;; bracketed paste：终端把整段粘贴包在 ESC[200~ ... ESC[201~ 之间。
    ;; Windows 控制台不走该协议，因此另有"输入突发"兜底。
    (define %paste-start "\x1b;[200~")
    (define %paste-end "\x1b;[201~")
    (define %paste-mode? #f)
    (define %pending-keys '())

    (define (terminal-paste-mode?)
      %paste-mode?
    ) ;define

    (define (terminal-set-paste-mode! enabled?)
      (set! %paste-mode? (and enabled? #t))
      %paste-mode?
    ) ;define

    (define (%wrap-paste text)
      (string-append %paste-start text %paste-end)
    ) ;define

    (define (%bytes-start-with? bytes prefix)
      (let loop
        ((bs bytes) (ps prefix))
        (cond ((null? ps) #t)
              ((null? bs) #f)
              ((= (car bs) (car ps)) (loop (cdr bs) (cdr ps)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; rev-bytes 以最新字节在前；expect 同样最新在前
    (define (%rev-ends-with? rev-bytes expect)
      (let loop
        ((rs rev-bytes) (es expect))
        (cond ((null? es) #t)
              ((null? rs) #f)
              ((= (car rs) (car es)) (loop (cdr rs) (cdr es)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%read-escape-bytes)
      (let loop
        ((bytes (list 27)))
        (if (console-kbhit? 10)
          (let ((b (console-getch)))
            (if (and b (exact-integer? b)) (loop (cons b bytes)) (reverse bytes))
          ) ;let
          (reverse bytes)
        ) ;if
      ) ;let
    ) ;define

    ;; ESC[200~ 之后的字节，读到 ESC[201~ 或超时为止
    (define (%read-bracketed-paste initial-bytes)
      (let loop
        ((acc (reverse initial-bytes)))
        (if (%rev-ends-with? acc '(126 49 48 50 91 27))
          (%wrap-paste
            (or (%resolve-utf8-bytes (reverse (drop acc 6))) "")
          ) ;%wrap-paste
          (if (console-kbhit? 50)
            (let ((b (console-getch)))
              (if (and b (exact-integer? b))
                (loop (cons b acc))
                (%wrap-paste (or (%resolve-utf8-bytes (reverse acc)) ""))
              ) ;if
            ) ;let
            (%wrap-paste (or (%resolve-utf8-bytes (reverse acc)) ""))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (%pending-push! key)
      (when key
        (set! %pending-keys (cons key %pending-keys))
      ) ;when
    ) ;define

    (define (%finish-paste-burst acc)
      (%wrap-paste (string-join (reverse acc) ""))
    ) ;define

    ;; Windows 兜底：首字符之后若队列里立刻还有输入，视为一次粘贴突发
    (define (%read-paste-burst first)
      (let loop
        ((acc (list first)))
        (if (console-kbhit? 10)
          (let ((c (console-getch)))
            (cond
             ((not (and c (exact-integer? c))) (%finish-paste-burst acc))
             ((= c 13) (loop (cons "\r" acc)))
             ((= c 10) (loop (cons "\n" acc)))
             ((= c 9) (loop (cons "\t" acc)))
             ((= c 27) (%pending-push! (%poll-escape-key)) (%finish-paste-burst acc))
             ((and (os-windows?) (or (= c 0) (= c 224)))
              (let ((ext (console-getch)))
                (when (and ext (exact-integer? ext))
                  (%pending-push! (%map-extended-key ext))
                ) ;when
              ) ;let
              (%finish-paste-burst acc)
             ) ;
             ((>= c 194)
              (let ((n (utf8-sequence-length c)))
                (if n
                  (let ((ch (%poll-utf8-char c n)))
                    (loop (if ch (cons ch acc) acc))
                  ) ;let
                  (loop acc)
                ) ;if
              ) ;let
             ) ;
             ((>= c 32) (loop (cons (string (integer->char c)) acc)))
             (else (%pending-push! (%map-standard-key c)) (%finish-paste-burst acc))
            ) ;cond
          ) ;let
          (%finish-paste-burst acc)
        ) ;if
      ) ;let
    ) ;define

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
          (let* ((bv (apply bytevector byte-list))
                 (str (utf8->string bv))
                 (k (parse-key str))
                ) ;
            (or k (parse-mouse-event str))
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
        (let ((bytes (%read-escape-bytes)))
          (if (and %paste-mode? (%bytes-start-with? bytes (list 27 91 50 48 48 126)))
            (%read-bracketed-paste (drop bytes 6))
            (%resolve-ansi-bytes bytes)
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%poll-console-key)
      (if (pair? %pending-keys)
        (let ((key (car %pending-keys)))
          (set! %pending-keys (cdr %pending-keys))
          key
        ) ;let
        (if (console-kbhit?)
          (let ((c (console-getch)))
            (let ((key
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
                  ) ;key
                 ) ;
              ;; 普通字符/回车之后紧跟更多输入：按粘贴突发处理（Windows 兜底）
              (if
                (and %paste-mode?
                  (or (eq? key 'enter)
                    (and (string? key)
                      (not (and (>= (string-length key) 6) (string=? (substring key 0 6) %paste-start))
                      ) ;not
                    ) ;and
                  ) ;or
                  (console-kbhit? 5)
                ) ;and
                (%read-paste-burst (if (eq? key 'enter) "\r" key))
                key
              ) ;if
            ) ;let
          ) ;let
          #f
        ) ;if
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
