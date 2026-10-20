;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui editor handle)
  (import (scheme base)
    (liii base)
    (liii tui component)
    (liii tui editor)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii pi modes tui editor types)
  ) ;import
  (export %pi-tui-editor-handle-key)
  (begin

    (define (%utf8-prev-char-col str col)
      (if (<= col 0)
        0
        (let loop
          ((k (- col 1)))
          (if (<= k 0)
            0
            (let ((b (char->integer (string-ref str k))))
              (if (and (>= b 128) (<= b 191)) (loop (- k 1)) k)
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%utf8-next-char-col str col)
      (let ((len (string-length str)))
        (if (>= col len)
          len
          (let* ((b (char->integer (string-ref str col)))
                 (step
                   (cond ((< b 128) 1)
                         ((and (>= b 192) (<= b 223)) 2)
                         ((and (>= b 224) (<= b 239)) 3)
                         ((and (>= b 240) (<= b 247)) 4)
                         (else 1)
                   ) ;cond
                 ) ;step
                ) ;
            (min len (+ col step))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%buffer-utf8-backspace! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
            ) ;
        (if (> c 0)
          (let* ((len (string-length line))
                 (prev-c (%utf8-prev-char-col line (min c len)))
                 (before (substring line 0 prev-c))
                 (after (substring line (min c len) len))
                 (new-line (string-append before after))
                 (new-lines
                   (let loop
                     ((rest lines) (i 0))
                     (cond ((null? rest) '())
                           ((= i r) (cons new-line (cdr rest)))
                           (else (cons (car rest) (loop (cdr rest) (+ i 1))))
                     ) ;cond
                   ) ;let
                 ) ;new-lines
                ) ;
            (buffer-lines-set! buf new-lines)
            (buffer-col-set! buf prev-c)
            #t
          ) ;let*
          (buffer-backspace! buf)
        ) ;if
      ) ;let*
    ) ;define

    (define (%buffer-utf8-delete! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (len (string-length line))
            ) ;
        (if (< c len)
          (let* ((next-c (%utf8-next-char-col line c))
                 (before (substring line 0 c))
                 (after (substring line next-c len))
                 (new-line (string-append before after))
                 (new-lines
                   (let loop
                     ((rest lines) (i 0))
                     (cond ((null? rest) '())
                           ((= i r) (cons new-line (cdr rest)))
                           (else (cons (car rest) (loop (cdr rest) (+ i 1))))
                     ) ;cond
                   ) ;let
                 ) ;new-lines
                ) ;
            (buffer-lines-set! buf new-lines)
            #t
          ) ;let*
          (buffer-delete! buf)
        ) ;if
      ) ;let*
    ) ;define

    (define (%buffer-utf8-move-left! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
            ) ;
        (if (> c 0)
          (let ((prev-c (%utf8-prev-char-col line (min c (string-length line)))))
            (buffer-col-set! buf prev-c)
            #t
          ) ;let
          (buffer-move-left! buf)
        ) ;if
      ) ;let*
    ) ;define

    (define (%buffer-utf8-move-right! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (len (string-length line))
            ) ;
        (if (< c len)
          (let ((next-c (%utf8-next-char-col line c)))
            (buffer-col-set! buf next-c)
            #t
          ) ;let
          (buffer-move-right! buf)
        ) ;if
      ) ;let*
    ) ;define

    (define (%handle-up comp buf hist on-ch)
      (if (zero? (buffer-row buf))
        (let ((prev-text (history-prev hist (buffer-text buf))))
          (if prev-text
            (begin
              (buffer-set-text! buf prev-text)
              (buffer-move-end! buf)
              (when (procedure? on-ch)
                (on-ch comp prev-text)
              ) ;when
              #t
            ) ;begin
            #t
          ) ;if
        ) ;let
        (buffer-move-up! buf)
      ) ;if
    ) ;define

    (define (%handle-down comp buf hist on-ch)
      (let* ((lines (buffer-lines buf)) (last-row (- (length lines) 1)))
        (if (>= (buffer-row buf) last-row)
          (let ((next-text (history-next hist (buffer-text buf))))
            (if next-text
              (begin
                (buffer-set-text! buf next-text)
                (buffer-move-end! buf)
                (when (procedure? on-ch)
                  (on-ch comp next-text)
                ) ;when
                #t
              ) ;begin
              #t
            ) ;if
          ) ;let
          (buffer-move-down! buf)
        ) ;if
      ) ;let*
    ) ;define

    (define (%handle-newline comp buf on-ch)
      (let ((res (buffer-newline! buf)))
        (when (procedure? on-ch)
          (on-ch comp (buffer-text buf))
        ) ;when
        #t
      ) ;let
    ) ;define

    (define (%handle-enter comp buf hist on-ch on-sub)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
            ) ;
        (if
          (and (> c 0) (char=? (string-ref line (- c 1)) #\\))
          (begin
            (buffer-backspace! buf)
            (%handle-newline comp buf on-ch)
          ) ;begin
          (let ((txt (buffer-text buf)))
            (history-push! hist txt)
            (when (procedure? on-sub)
              (on-sub comp txt)
            ) ;when
            #t
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%pi-tui-editor-handle-key comp key)
      (let* ((data (component-data comp))
             (buf (ed-data-buffer data))
             (hist (ed-data-history data))
             (on-ch (ed-data-on-change data))
             (on-sub (ed-data-on-submit data))
            ) ;
        (cond ((eq? key 'left) (%buffer-utf8-move-left! buf))
              ((eq? key 'right) (%buffer-utf8-move-right! buf))
              ((eq? key 'home) (buffer-move-home! buf))
              ((eq? key 'end) (buffer-move-end! buf))
              ((eq? key 'up) (%handle-up comp buf hist on-ch))
              ((eq? key 'down) (%handle-down comp buf hist on-ch))
              ((eq? key 'backspace)
               (let ((res (%buffer-utf8-backspace! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((eq? key 'delete)
               (let ((res (%buffer-utf8-delete! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((or (eq? key 'shift-enter)
                 (eq? key 'alt-enter)
                 (eq? key 'ctrl-enter)
                 (equal? key "\n")
               ) ;or
               (%handle-newline comp buf on-ch)
              ) ;
              ((eq? key 'enter) (%handle-enter comp buf hist on-ch on-sub))
              ((string? key)
               (buffer-insert-string! buf key)
               (when (procedure? on-ch)
                 (on-ch comp (buffer-text buf))
               ) ;when
               #t
              ) ;
              ((char? key)
               (buffer-insert-char! buf key)
               (when (procedure? on-ch)
                 (on-ch comp (buffer-text buf))
               ) ;when
               #t
              ) ;
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
