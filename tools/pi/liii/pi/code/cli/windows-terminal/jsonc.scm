;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli windows-terminal jsonc)
  (export jsonc-ws? jsonc-skip-comment-at jsonc-skip-ws-and-comments
    jsonc-next-str-state wt-detect-alt-enter-status
  ) ;export
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii string)
    (liii list)
    (liii json)
  ) ;import
  (begin

    (define (jsonc-ws? c)
      (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\newline)
        (char=? c #\return)
      ) ;or
    ) ;define

    (define (%skip-ws-at str j len)
      (let loop
        ((k j))
        (if (>= k len) k (if (jsonc-ws? (string-ref str k)) (loop (+ k 1)) k))
      ) ;let
    ) ;define

    (define (%skip-line-comment str j len)
      (let loop
        ((k (+ j 2)))
        (cond ((>= k len) k)
              ((char=? (string-ref str k) #\newline) (+ k 1))
              (else (loop (+ k 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%skip-block-comment str j len)
      (let loop
        ((k (+ j 2)))
        (cond ((>= (+ k 1) len) len)
              ((and (char=? (string-ref str k) #\*) (char=? (string-ref str (+ k 1)) #\/))
               (+ k 2)
              ) ;
              (else (loop (+ k 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (jsonc-skip-comment-at str k len)
      (if
        (or (>= (+ k 1) len) (not (char=? (string-ref str k) #\/)))
        #f
        (let ((c2 (string-ref str (+ k 1))))
          (cond ((char=? c2 #\/) (%skip-line-comment str k len))
                ((char=? c2 #\*) (%skip-block-comment str k len))
                (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (jsonc-skip-ws-and-comments str start len)
      (let loop
        ((i start))
        (if (>= i len)
          len
          (let ((c (string-ref str i)))
            (cond ((jsonc-ws? c) (loop (+ i 1)))
                  ((char=? c #\/)
                   (let ((next-i (jsonc-skip-comment-at str i len)))
                     (if next-i (loop next-i) i)
                   ) ;let
                  ) ;
                  (else i)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%is-trailing-comma? str start len)
      (let loop
        ((j start))
        (let ((k (%skip-ws-at str j len)))
          (if (>= k len)
            #f
            (let ((c (string-ref str k)))
              (cond
               ((and (char=? c #\/) (< (+ k 1) len))
                (let ((next-k (jsonc-skip-comment-at str k len)))
                  (if next-k (loop next-k) #f)
                ) ;let
               ) ;
               ((or (char=? c #\]) (char=? c #\})) #t)
               (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (jsonc-next-str-state c esc)
      (cond (esc (values #t #f))
            ((char=? c #\\) (values #t #t))
            ((char=? c #\") (values #f #f))
            (else (values #t #f))
      ) ;cond
    ) ;define
    (define (%clean-handle-slash str i len out)
      (let ((c2 (string-ref str (+ i 1))))
        (cond ((char=? c2 #\/) (write-char #\newline out) (%skip-line-comment str i len))
              ((char=? c2 #\*) (write-char #\space out) (%skip-block-comment str i len))
              (else (write-char (string-ref str i) out) (+ i 1))
        ) ;cond
      ) ;let
    ) ;define
    (define (jsonc->clean-json str)
      (let ((len (string-length str)) (out (open-output-string)))
        (let loop
          ((i 0) (in-str #f) (esc #f))
          (if (>= i len)
            (get-output-string out)
            (let ((c (string-ref str i)))
              (if in-str
                (begin
                  (write-char c out)
                  (let-values (((next-str next-esc) (jsonc-next-str-state c esc)))
                    (loop (+ i 1) next-str next-esc)
                  ) ;let-values
                ) ;begin
                (cond ((char=? c #\") (write-char c out) (loop (+ i 1) #t #f))
                      ((and (char=? c #\/) (< (+ i 1) len))
                       (let ((next-i (%clean-handle-slash str i len out)))
                         (loop next-i #f #f)
                       ) ;let
                      ) ;
                      ((char=? c #\,)
                       (if (%is-trailing-comma? str (+ i 1) len)
                         (loop (+ i 1) #f #f)
                         (begin
                           (write-char c out)
                           (loop (+ i 1) #f #f)
                         ) ;begin
                       ) ;if
                      ) ;
                      (else (write-char c out) (loop (+ i 1) #f #f))
                ) ;cond
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (%alist-get-ci alist key-str)
      (if (not (pair? alist))
        #f
        (let loop
          ((pairs alist))
          (if (null? pairs)
            #f
            (let ((entry (car pairs)))
              (if (pair? entry)
                (let ((k (car entry)))
                  (cond
                   ((string? k) (if (string-ci=? k key-str) (cdr entry) (loop (cdr pairs))))
                   ((symbol? k)
                    (if (string-ci=? (symbol->string k) key-str) (cdr entry) (loop (cdr pairs)))
                   ) ;
                   (else (loop (cdr pairs)))
                  ) ;cond
                ) ;let
                (loop (cdr pairs))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define
    (define (%normalize-key-str s)
      (let* ((lower (string-downcase s)) (replaced (string-replace lower "return" "enter")))
        (string-replace replaced " " "")
      ) ;let*
    ) ;define
    (define (%key-matches-alt-enter? key-val)
      (cond
       ((string? key-val)
        (let ((norm (%normalize-key-str key-val)))
          (or (string=? norm "alt+enter") (string=? norm "enter+alt"))
        ) ;let
       ) ;
       ((vector? key-val) (any %key-matches-alt-enter? (vector->list key-val)))
       ((pair? key-val) (any %key-matches-alt-enter? key-val))
       (else #f)
      ) ;cond
    ) ;define
    (define (%cmd-string-val cmd)
      (cond ((string? cmd) (string-downcase (string-trim-both cmd)))
            ((pair? cmd)
             (let ((sub (or (%alist-get-ci cmd "action") (%alist-get-ci cmd "command"))))
               (if (string? sub) (string-downcase (string-trim-both sub)) "")
             ) ;let
            ) ;
            (else "")
      ) ;cond
    ) ;define
    (define (%cmd->status cmd)
      (let ((cmd-str (%cmd-string-val cmd)))
        (cond ((string=? cmd-str "unbound") 'already-unbound)
              ((or (string=? cmd-str "togglefullscreen")
                 (string=? cmd-str "toggle-fullscreen")
               ) ;or
               'default-conflict
              ) ;
              (else 'user-custom)
        ) ;cond
      ) ;let
    ) ;define
    (define (%to-list val)
      (cond ((vector? val) (vector->list val))
            ((pair? val) val)
            (else '())
      ) ;cond
    ) ;define
    (define (%scan-action-items items)
      (let loop
        ((rem items) (effective #f))
        (if (null? rem)
          (or effective 'default-conflict)
          (let ((item (car rem)))
            (if (pair? item)
              (let ((keys (%alist-get-ci item "keys")))
                (if (%key-matches-alt-enter? keys)
                  (let ((cmd (or (%alist-get-ci item "command") (%alist-get-ci item "action"))))
                    (loop (cdr rem) (%cmd->status cmd))
                  ) ;let
                  (loop (cdr rem) effective)
                ) ;if
              ) ;let
              (loop (cdr rem) effective)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%detect-status-from-parsed parsed)
      (let* ((actions (%to-list (%alist-get-ci parsed "actions")))
             (keybindings (%to-list (%alist-get-ci parsed "keybindings")))
             (all-items (append keybindings actions))
            ) ;
        (%scan-action-items all-items)
      ) ;let*
    ) ;define
    (define (wt-detect-alt-enter-status text)
      (catch #t
        (lambda ()
          (let* ((clean (jsonc->clean-json text)) (parsed (string->json clean)))
            (if (not (pair? parsed)) 'default-conflict (%detect-status-from-parsed parsed))
          ) ;let*
        ) ;lambda
        (lambda _ 'default-conflict)
      ) ;catch
    ) ;define
  ) ;begin
) ;define-library
