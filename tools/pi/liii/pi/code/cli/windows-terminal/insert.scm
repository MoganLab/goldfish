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

    (define (%find-root-open text len)
      (let loop
        ((i 0) (in-str #f) (esc #f))
        (if (>= i len)
          #f
          (let ((c (string-ref text i)))
            (if in-str
              (let-values (((next-in next-esc) (jsonc-next-str-state c esc)))
                (loop (+ i 1) next-in next-esc)
              ) ;let-values
              (let ((next-i (jsonc-skip-comment-at text i len)))
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

    (define (%find-matching-close text open-pos limit open-ch close-ch)
      (let ((len (string-length text)))
        (let loop
          ((i (+ open-pos 1)) (depth 1) (in-str #f) (esc #f))
          (if (>= i limit)
            #f
            (let ((c (string-ref text i)))
              (if in-str
                (let-values (((next-in next-esc) (jsonc-next-str-state c esc)))
                  (loop (+ i 1) depth next-in next-esc)
                ) ;let-values
                (let ((next-i (jsonc-skip-comment-at text i len)))
                  (cond (next-i (loop next-i depth #f #f))
                        ((char=? c #\") (loop (+ i 1) depth #t #f))
                        ((char=? c open-ch) (loop (+ i 1) (+ depth 1) #f #f))
                        ((char=? c close-ch) (if (= depth 1) i (loop (+ i 1) (- depth 1) #f #f)))
                        (else (loop (+ i 1) depth #f #f))
                  ) ;cond
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%find-root-braces text)
      (let* ((len (string-length text)) (open-pos (%find-root-open text len)))
        (if (not open-pos)
          (cons #f #f)
          (cons open-pos (%find-matching-close text open-pos len #\{ #\}))
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
                  (let-values (((next-in next-esc) (jsonc-next-str-state c esc)))
                    (loop (+ i 1) next-in next-esc)
                  ) ;let-values
                ) ;begin
                (let ((next-i (jsonc-skip-comment-at text i len)))
                  (cond (next-i (loop next-i #f #f))
                        ((jsonc-ws? c) (loop (+ i 1) #f #f))
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

    (define (%find-prop-array text after-key root-close)
      (let ((after-colon (jsonc-skip-ws-and-comments text after-key root-close)))
        (if (and (< after-colon root-close) (char=? (string-ref text after-colon) #\:))
          (let ((arr-start (jsonc-skip-ws-and-comments text (+ after-colon 1) root-close)))
            (if (and (< arr-start root-close) (char=? (string-ref text arr-start) #\[))
              (let ((arr-end (%find-matching-close text arr-start root-close #\[ #\])))
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

    (define (%handle-root-key text i root-close actions-info kb-info)
      (let-values (((kname after-key) (%read-key-string text i root-close)))
        (if (not kname)
          (values actions-info kb-info after-key)
          (let-values (((a-start a-end next-idx) (%find-prop-array text after-key root-close)))
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
      (let ((actions-info #f) (kb-info #f))
        (let loop
          ((i (+ root-open 1)) (depth 1) (b-depth 0) (in-str #f) (esc #f))
          (if (>= i root-close)
            (or actions-info kb-info)
            (let ((c (string-ref text i)))
              (if in-str
                (let-values (((next-in next-esc) (jsonc-next-str-state c esc)))
                  (loop (+ i 1) depth b-depth next-in next-esc)
                ) ;let-values
                (let ((next-i (jsonc-skip-comment-at text i root-close)))
                  (cond (next-i (loop next-i depth b-depth #f #f))
                        ((and (= depth 1) (= b-depth 0) (char=? c #\"))
                         (let-values (((act-info new-kb-info next-idx)
                                       (%handle-root-key text i root-close actions-info kb-info)
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

    (define (%insert-payload text open close empty-insertion nonempty-insertion)
      (let ((last-pos (%find-last-significant-pos text (+ open 1) close)))
        (if (not last-pos)
          (string-append (substring text 0 close) empty-insertion (substring text close))
          (if (char=? (string-ref text last-pos) #\,)
            (string-append (substring text 0 close)
              nonempty-insertion
              (substring text close)
            ) ;string-append
            (string-append (substring text 0 (+ last-pos 1))
              ","
              (substring text (+ last-pos 1) close)
              nonempty-insertion
              (substring text close)
            ) ;string-append
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (%insert-into-array text open close)
      (%insert-payload text open close
        "\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    "
        "    { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    "
      ) ;%insert-payload
    ) ;define

    (define (%insert-into-root text open close)
      (%insert-payload text open close
        "\n    \"actions\": [\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    ]\n"
        "    \"actions\": [\n        { \"command\": \"unbound\", \"keys\": \"alt+enter\" }\n    ]\n"
      ) ;%insert-payload
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
