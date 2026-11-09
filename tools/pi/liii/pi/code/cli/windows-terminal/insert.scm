;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli windows-terminal insert)
  (export wt-insert-unbound-action)
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii string)
    (liii list)
    (liii pi code cli windows-terminal jsonc)
  ) ;import
  (begin

    (define (%skip-comment-at str i len)
      (if
        (or (not (char=? (string-ref str i) #\/)) (>= (+ i 1) len))
        #f
        (let ((c2 (string-ref str (+ i 1))))
          (cond
           ((char=? c2 #\/)
            (let loop
              ((j (+ i 2)))
              (cond ((>= j len) len)
                    ((char=? (string-ref str j) #\newline) (+ j 1))
                    (else (loop (+ j 1)))
              ) ;cond
            ) ;let
           ) ;
           ((char=? c2 #\*)
            (let loop
              ((j (+ i 2)))
              (cond ((>= (+ j 1) len) len)
                    ((and (char=? (string-ref str j) #\*) (char=? (string-ref str (+ j 1)) #\/))
                     (+ j 2)
                    ) ;
                    (else (loop (+ j 1)))
              ) ;cond
            ) ;let
           ) ;
           (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%skip-ws-and-comments str start len)
      (let loop
        ((i start))
        (if (>= i len)
          len
          (let ((c (string-ref str i)))
            (cond ((or (char=? c #\space)
                     (char=? c #\tab)
                     (char=? c #\newline)
                     (char=? c #\return)
                   ) ;or
                   (loop (+ i 1))
                  ) ;
                  ((char=? c #\/)
                   (let ((next-i (%skip-comment-at str i len)))
                     (if next-i (loop next-i) i)
                   ) ;let
                  ) ;
                  (else i)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%next-str-state c esc)
      (cond (esc (values #t #f))
            ((char=? c #\\) (values #t #t))
            ((char=? c #\") (values #f #f))
            (else (values #t #f))
      ) ;cond
    ) ;define
    (define (%find-array-close text open-idx root-close len)
      (let loop
        ((k (+ open-idx 1)) (b-depth 1) (in-str #f) (esc #f))
        (if (>= k root-close)
          #f
          (let ((c (string-ref text k)))
            (if in-str
              (let-values (((next-in next-esc) (%next-str-state c esc)))
                (loop (+ k 1) b-depth next-in next-esc)
              ) ;let-values
              (let ((next-k (%skip-comment-at text k len)))
                (cond (next-k (loop next-k b-depth #f #f))
                      ((char=? c #\") (loop (+ k 1) b-depth #t #f))
                      ((char=? c #\[) (loop (+ k 1) (+ b-depth 1) #f #f))
                      ((char=? c #\]) (if (= b-depth 1) k (loop (+ k 1) (- b-depth 1) #f #f)))
                      (else (loop (+ k 1) b-depth #f #f))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%find-root-open text len)
      (let loop
        ((i 0) (in-str #f) (esc #f))
        (if (>= i len)
          #f
          (let ((c (string-ref text i)))
            (if in-str
              (let-values (((next-in next-esc) (%next-str-state c esc)))
                (loop (+ i 1) next-in next-esc)
              ) ;let-values
              (let ((next-i (%skip-comment-at text i len)))
                (cond (next-i (loop next-i #f #f))
                      ((char=? c #\") (loop (+ i 1) #t #f))
                      ((char=? c #\{) i)
                      (else (loop (+ i 1) #f #f))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%find-root-close text open-pos len)
      (let loop
        ((j (+ open-pos 1)) (depth 1) (in-str #f) (esc #f))
        (if (>= j len)
          #f
          (let ((c (string-ref text j)))
            (if in-str
              (let-values (((next-in next-esc) (%next-str-state c esc)))
                (loop (+ j 1) depth next-in next-esc)
              ) ;let-values
              (let ((next-j (%skip-comment-at text j len)))
                (cond (next-j (loop next-j depth #f #f))
                      ((char=? c #\") (loop (+ j 1) depth #t #f))
                      ((char=? c #\{) (loop (+ j 1) (+ depth 1) #f #f))
                      ((char=? c #\}) (if (= depth 1) j (loop (+ j 1) (- depth 1) #f #f)))
                      (else (loop (+ j 1) depth #f #f))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%find-root-braces text)
      (let* ((len (string-length text)) (open-pos (%find-root-open text len)))
        (if (not open-pos)
          (cons #f #f)
          (cons open-pos (%find-root-close text open-pos len))
        ) ;if
      ) ;let*
    ) ;define
    (define (%find-last-significant-pos text start end)
      (let ((len (string-length text)) (last-pos #f))
        (let loop
          ((i start) (in-str #f) (esc #f))
          (if (>= i end)
            last-pos
            (let ((c (string-ref text i)))
              (if in-str
                (begin
                  (set! last-pos i)
                  (let-values (((next-in next-esc) (%next-str-state c esc)))
                    (loop (+ i 1) next-in next-esc)
                  ) ;let-values
                ) ;begin
                (let ((next-i (%skip-comment-at text i len)))
                  (cond (next-i (loop next-i #f #f))
                        ((or (char=? c #\space)
                           (char=? c #\tab)
                           (char=? c #\newline)
                           (char=? c #\return)
                         ) ;or
                         (loop (+ i 1) #f #f)
                        ) ;
                        (else (set! last-pos i) (loop (+ i 1) (char=? c #\") #f))
                  ) ;cond
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (%read-key-string text i root-close)
      (let loop
        ((j (+ i 1)) (k-chars '()))
        (cond ((>= j root-close) (values #f j))
              ((char=? (string-ref text j) #\\)
               (loop (+ j 2) (cons (string-ref text (+ j 1)) k-chars))
              ) ;
              ((char=? (string-ref text j) #\")
               (values (list->string (reverse k-chars)) (+ j 1))
              ) ;
              (else (loop (+ j 1) (cons (string-ref text j) k-chars)))
        ) ;cond
      ) ;let
    ) ;define
    (define (%find-prop-array text after-key root-close len)
      (let ((after-colon (%skip-ws-and-comments text after-key root-close)))
        (if (and (< after-colon root-close) (char=? (string-ref text after-colon) #\:))
          (let ((arr-start (%skip-ws-and-comments text (+ after-colon 1) root-close)))
            (if (and (< arr-start root-close) (char=? (string-ref text arr-start) #\[))
              (let ((arr-end (%find-array-close text arr-start root-close len)))
                (if arr-end
                  (values arr-start arr-end (+ arr-end 1))
                  (values #f #f (+ arr-start 1))
                ) ;if
              ) ;let
              (values #f #f after-key)
            ) ;if
          ) ;let
          (values #f #f after-key)
        ) ;if
      ) ;let
    ) ;define
    (define (%handle-root-key text i root-close len actions-info kb-info)
      (let-values (((kname after-key) (%read-key-string text i root-close)))
        (if (not kname)
          (values actions-info kb-info after-key)
          (let-values (((a-start a-end next-idx) (%find-prop-array text after-key root-close len)))
            (cond ((and a-start (string-ci=? kname "actions"))
                   (values (list 'actions a-start a-end) kb-info next-idx)
                  ) ;
                  ((and a-start (string-ci=? kname "keybindings"))
                   (values actions-info (list 'keybindings a-start a-end) next-idx)
                  ) ;
                  (else (values actions-info kb-info next-idx))
            ) ;cond
          ) ;let-values
        ) ;if
      ) ;let-values
    ) ;define
    (define (%find-target-array text root-open root-close)
      (let ((len (string-length text)) (actions-info #f) (kb-info #f))
        (let loop
          ((i (+ root-open 1)) (depth 1) (b-depth 0) (in-str #f) (esc #f))
          (if (or (>= i root-close) (and (<= depth 0) (<= b-depth 0)))
            (or actions-info kb-info)
            (let ((c (string-ref text i)))
              (if in-str
                (let-values (((next-in next-esc) (%next-str-state c esc)))
                  (loop (+ i 1) depth b-depth next-in next-esc)
                ) ;let-values
                (let ((next-i (%skip-comment-at text i len)))
                  (cond (next-i (loop next-i depth b-depth #f #f))
                        ((and (= depth 1) (= b-depth 0) (char=? c #\"))
                         (let-values (((act-info new-kb-info next-idx)
                                       (%handle-root-key text i root-close len actions-info kb-info)
                                      ) ;
                                     ) ;
                           (set! actions-info act-info)
                           (set! kb-info new-kb-info)
                           (loop next-idx depth b-depth #f #f)
                         ) ;let-values
                        ) ;
                        ((char=? c #\{) (loop (+ i 1) (+ depth 1) b-depth #f #f))
                        ((char=? c #\}) (loop (+ i 1) (- depth 1) b-depth #f #f))
                        ((char=? c #\[) (loop (+ i 1) depth (+ b-depth 1) #f #f))
                        ((char=? c #\]) (loop (+ i 1) depth (- b-depth 1) #f #f))
                        (else (loop (+ i 1) depth b-depth #f #f))
                  ) ;cond
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (%insert-into-array text open-bracket close-bracket)
      (let ((last-pos (%find-last-significant-pos text (+ open-bracket 1) close-bracket)))
        (if (not last-pos)
          (string-append (substring text 0 close-bracket)
            "\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    "
            (substring text close-bracket)
          ) ;string-append
          (let ((c (string-ref text last-pos)))
            (if (char=? c #\,)
              (string-append (substring text 0 close-bracket)
                "    { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    "
                (substring text close-bracket)
              ) ;string-append
              (string-append (substring text 0 (+ last-pos 1))
                ","
                (substring text (+ last-pos 1) close-bracket)
                "    { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    "
                (substring text close-bracket)
              ) ;string-append
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%insert-into-root text root-open root-close)
      (let ((last-pos (%find-last-significant-pos text (+ root-open 1) root-close)))
        (if (not last-pos)
          (string-append (substring text 0 root-close)
            "\n    \"actions\": [\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    ]\n"
            (substring text root-close)
          ) ;string-append
          (let ((c (string-ref text last-pos)))
            (if (char=? c #\,)
              (string-append (substring text 0 root-close)
                "    \"actions\": [\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    ]\n"
                (substring text root-close)
              ) ;string-append
              (string-append (substring text 0 (+ last-pos 1))
                ","
                (substring text (+ last-pos 1) root-close)
                "    \"actions\": [\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    ]\n"
                (substring text root-close)
              ) ;string-append
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (wt-insert-unbound-action text)
      (let ((status (wt-detect-alt-enter-status text)))
        (if (or (eq? status 'already-unbound) (eq? status 'user-custom))
          text
          (let* ((braces (%find-root-braces text))
                 (root-open (car braces))
                 (root-close (cdr braces))
                ) ;
            (if (or (not root-open) (not root-close))
              text
              (let ((target-array (%find-target-array text root-open root-close)))
                (if target-array
                  (%insert-into-array text (cadr target-array) (caddr target-array))
                  (%insert-into-root text root-open root-close)
                ) ;if
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
