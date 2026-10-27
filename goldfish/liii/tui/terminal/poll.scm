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
    %map-standard-key %string-chunks %split-paste-end %reverse-paste-end?
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
  ) ;import
  (begin

    ;; bracketed paste：终端把整段粘贴包在 ESC[200~ ... ESC[201~ 之间。
    ;; 统一包装成同一个字符串交给 editor，由 editor 决定内联还是折叠。
    ;; 默认关闭：只有显式开启的 TUI（如 gf pi）才会收到粘贴事件，
    ;; 避免影响仍按普通按键处理输入的其他消费者（如 gf repl）。
    (define %paste-start "\x1b;[200~")
    (define %paste-end "\x1b;[201~")
    (define %paste-mode? #f)

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

    ;; 按码点拆成单字符块，便于检查粘贴结束标记
    (define (%string-chunks text)
      (let loop
        ((i 0) (acc '()))
        (if (>= i (utf8-string-length text))
          (reverse acc)
          (loop (+ i 1) (cons (utf8-substring text i (+ i 1)) acc))
        ) ;if
      ) ;let
    ) ;define

    ;; acc 以最新字符在前；检查末尾 6 个字符是否为 ESC[201~
    (define (%reverse-paste-end? acc)
      (let loop
        ((rest acc) (expect '("~" "1" "0" "2" "[" "\x1b;")))
        (cond ((null? expect) #t)
              ((null? rest) #f)
              ((string=? (car rest) (car expect)) (loop (cdr rest) (cdr expect)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 在正序字符块中查找 ESC[201~，返回 (content . trailing) 或 #f
    (define (%split-paste-end chunks)
      (let loop
        ((rest chunks) (acc '()))
        (if (null? rest)
          #f
          (let ((marker?
                  (let check
                    ((rs rest) (expect '("\x1b;" "[" "2" "0" "1" "~")))
                    (cond ((null? expect) #t)
                          ((null? rs) #f)
                          ((string=? (car rs) (car expect)) (check (cdr rs) (cdr expect)))
                          (else #f)
                    ) ;cond
                  ) ;let
                ) ;marker?
               ) ;
            (if marker?
              (cons (string-join (reverse acc) "") (string-join (drop rest 6) ""))
              (loop (cdr rest) (cons (car rest) acc))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 读取 ESC[200~ 之后的全部内容，直到 ESC[201~。
    ;; 超时仍未读完时返回已收到的部分，避免吞掉用户输入。
    ;; 用块列表累积，避免逐字符 string-append 的 O(n^2) 拷贝。
    (define (%read-paste-body initial)
      (let loop
        ((acc (reverse (%string-chunks initial))))
        (if (%reverse-paste-end? acc)
          (%wrap-paste (string-join (reverse (drop acc 6)) ""))
          (if (console-kbhit? 50)
            (let ((c (console-getch)))
              (cond
               ((not (and c (exact-integer? c))) (%wrap-paste (string-join (reverse acc) "")))
               ;; 保留换行与 Tab，交给 %paste-normalize 统一处理
               ((= c 13) (loop (cons "\r" acc)))
               ((= c 10) (loop (cons "\n" acc)))
               ((= c 9) (loop (cons "\t" acc)))
               ((and (os-windows?) (>= c 32)) (loop (cons (string (integer->char c)) acc)))
               ((and (>= c 194) (<= c 255))
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
               (else (loop acc))
              ) ;cond
            ) ;let
            (%wrap-paste (string-join (reverse acc) ""))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (%poll-escape-key)
      (if (not (console-kbhit? 25))
        'escape
        (let* ((bytes (%read-escape-bytes)) (str (%resolve-utf8-bytes bytes)))
          (if (and %paste-mode? str (string-starts? str %paste-start))
            (let* ((rest (substring str 6)) (split (%split-paste-end (%string-chunks rest))))
              (if split (%wrap-paste (car split)) (%read-paste-body rest))
            ) ;let*
            (%resolve-ansi-bytes bytes)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; 突发收集时若碰到方向键等转义序列，先解析成普通按键暂存，
    ;; 避免把 ESC 序列的字节吞进粘贴文本。
    (define %pending-keys '())

    (define (%pending-push! key)
      (when key
        (set! %pending-keys (cons key %pending-keys))
      ) ;when
    ) ;define

    ;; Windows 控制台不走 bracketed paste：首字符之后若队列里立刻还有按键，
    ;; 视为一次粘贴突发，连续收集到队列暂时为空为止。
    (define (%finish-paste-burst acc)
      (%wrap-paste (string-join (reverse acc) ""))
    ) ;define

    (define (%read-paste-burst first)
      (let loop
        ((acc (list first)))
        (if (console-kbhit? 10)
          (let ((c (console-getch)))
            (cond
             ((not (and c (exact-integer? c))) (%finish-paste-burst acc))
             ;; 保留原始 CR/LF/Tab，交给 %paste-normalize 统一处理，
             ;; 避免 Windows 的 CRLF 被收集成两个换行
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
             ((and (os-windows?) (>= c 32)) (loop (cons (string (integer->char c)) acc)))
             ((and (>= c 194) (<= c 255))
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
              ;; 普通字符/回车之后紧跟大量输入：按粘贴突发处理（Windows 兜底）
              (if (and %paste-mode? (or (string? key) (eq? key 'enter)) (console-kbhit? 1))
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
