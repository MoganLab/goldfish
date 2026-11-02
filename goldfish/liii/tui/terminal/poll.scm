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
    %map-standard-key %poll-mouse-feed %poll-mouse-reset!
    terminal-drain-mouse-events
  ) ;export
  (import (scheme base)
    (liii error)
    (liii list)
    (liii os)
    (liii string)
    (liii unicode)
    (liii date-time)
    (liii tui terminal protocol)
    (liii tui terminal virtual)
    (liii tui terminal key-map)
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

    ;; CSI 终结字节（0x40~0x7E）：序列到此结束
    (define (%csi-final-byte? b)
      (and (integer? b) (>= b 64) (<= b 126))
    ) ;define

    ;; 转义序列是否已完整（bytes 按到达顺序排列）：
    ;;   ESC [ ... <终结字节>  → CSI 序列（鼠标 SGR 事件即此类）
    ;;   ESC O <终结字节>      → SS3 序列
    ;;   ESC <字符>            → 单字符转义
    ;; 序列完整即可停止读取：否则每个转义序列都要在末尾多等一次超时
    ;; （鼠标滚轮事件因此凭空多出 10ms 延迟）。
    (define (%escape-bytes-complete? bytes)
      (let ((len (length bytes)))
        (cond ((< len 2) #f)
              ((= (cadr bytes) 91)
               (and (> len 2) (%csi-final-byte? (list-ref bytes (- len 1))))
              ) ;
              ((= (cadr bytes) 79) (> len 2))
              (else #t)
        ) ;cond
      ) ;let
    ) ;define

    ;; 读取一个转义序列的字节：按到达顺序累积，序列完整即返回
    (define (%read-escape-bytes)
      (let loop
        ((bytes (list 27)))
        (if (console-kbhit? 10)
          (let ((b (console-getch)))
            (if (and b (exact-integer? b))
              (let ((bytes (append bytes (list b))))
                (if (%escape-bytes-complete? bytes) bytes (loop bytes))
              ) ;let
              bytes
            ) ;if
          ) ;let
          bytes
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

    ;; click 合成状态：最近一次 press 事件（#f 表示无）
    (define %mouse-press-state #f)

    ;; press 与同位置同按钮 release 合成 click（对齐上游 pi 的合成机制），
    ;; 其他事件透传并按需更新 press 状态
    (define (%synthesize-click ev)
      (let ((t (mouse-event-type ev)))
        (cond ((eq? t 'press) (set! %mouse-press-state ev) ev)
              ((eq? t 'release)
               (let* ((p %mouse-press-state)
                      (click? (and p
                                (eq? (mouse-event-button p) (mouse-event-button ev))
                                (= (mouse-event-x p) (mouse-event-x ev))
                                (= (mouse-event-y p) (mouse-event-y ev))
                              ) ;and
                      ) ;click?
                     ) ;
                 (set! %mouse-press-state #f)
                 (if click?
                   (make-mouse-event 'click
                     (mouse-event-button ev)
                     (mouse-event-x ev)
                     (mouse-event-y ev)
                     (mouse-event-shift? ev)
                     (mouse-event-ctrl? ev)
                     (mouse-event-alt? ev)
                     (mouse-event-time ev)
                   ) ;make-mouse-event
                   ev
                 ) ;if
               ) ;let*
              ) ;
              (else ev)
        ) ;cond
      ) ;let
    ) ;define

    (define (%resolve-ansi-bytes byte-list)
      (if (null? byte-list)
        #f
        (if (<= (length byte-list) 1)
          'escape
          (let* ((bv (apply bytevector byte-list))
                 (str (utf8->string bv))
                 (k (parse-key str))
                 ;; 事件到达时刻：读取到完整序列时打戳，供滚轮加速按真实间隔换算
                 (ev (or k (parse-mouse-event str (current-monotonic-millis))))
                ) ;
            (if (mouse-event? ev) (%synthesize-click ev) ev)
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    ;; 测试接缝：直接注入字节流驱动 click 合成；reset 清空 press 状态
    (define (%poll-mouse-feed byte-list)
      (%resolve-ansi-bytes byte-list)
    ) ;define

    (define (%poll-mouse-reset!)
      (set! %mouse-press-state #f)
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

    ;; 把一个按键原样放回输入队列头部（不丢键、不改顺序）：
    ;; 标准终端走 %pending-keys 栈（只回填一个键，出栈即恢复原序），
    ;; 虚拟终端走 unshift（插回队首）。
    (define (%unshift-key! term key)
      (if (and term (virtual-terminal? term))
        (virtual-terminal-unshift-key! term key)
        (%pending-push! key)
      ) ;if
    ) ;define

    ;; 一次性取走当前已到达的所有鼠标事件（非阻塞）。
    ;; 遇到非鼠标按键立即停止，并把它原样放回输入队列头部：
    ;; 既不丢按键，也不改变按键顺序。
    ;; 典型用法：滚轮事件连续到达时一次取走、合并成一次滚动，
    ;; 避免单事件渲染耗时大于事件到达间隔时输入不断积压（越滚越慢）。
    (define (terminal-drain-mouse-events . args)
      (let ((term (if (null? args) #f (car args))))
        (if
          (and term (not (or (virtual-terminal? term) (terminal? term))))
          (type-error "terminal-drain-mouse-events: expected a terminal or #f")
          (let loop
            ((acc '()))
            (let ((k (terminal-poll-key term)))
              (cond ((not k) (reverse acc))
                    ((mouse-event? k) (loop (cons k acc)))
                    (else (%unshift-key! term k) (reverse acc))
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
