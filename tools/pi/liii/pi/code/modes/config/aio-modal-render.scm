;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` all-in-one 子标签页交互覆盖层渲染。
(define-library (liii pi code modes config aio-modal-render)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui)
    (liii pi ai thinking)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-aio-modal-render)
  (begin

    (define %accent "\x1b;[35m")
    (define %reset "\x1b;[0m")
    (define %dim "\x1b;[2m")
    (define %selected-style "\x1b;[48;2;74;44;122m\x1b;[38;2;244;240;255m")

    (define (%ref alist key default)
      (let ((pair (and (pair? alist) (assoc key alist))))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%clamp idx count)
      (cond ((<= count 0) 0)
            ((< idx 0) 0)
            ((>= idx count) (- count 1))
            (else idx)
      ) ;cond
    ) ;define

    (define (%fit text width)
      (let* ((plain (strip-ansi text)) (len (display-width plain)))
        (cond ((= width 0) "")
              ((<= len width) (string-append text (utf8-make-string (- width len) #\space)))
              ((<= (string-length plain) width) plain)
              (else (truncate-to-width plain width))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%pad lines rows)
      (let ((n (length lines)))
        (cond ((= n rows) lines)
              ((> n rows) (take lines rows))
              (else
                (append lines (map (lambda (i) "") (iota (- rows n))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%window-start total count selected)
      (if (<= total count)
        0
        (max 0 (min (- total count) (- selected (quotient count 2))))
      ) ;if
    ) ;define

    (define (%render-items items selected rows label-fn inner)
      (let* ((total (length items))
             (start (%window-start total rows selected))
             (visible (list-tail items start))
             (trimmed (if (> (length visible) rows) (take visible rows) visible))
            ) ;
        (let loop
          ((rest trimmed) (idx start) (acc '()))
          (if (null? rest)
            (reverse acc)
            (let ((txt (%fit (label-fn (car rest)) inner)))
              (loop (cdr rest) (+ idx 1) (cons (cons (= idx selected) txt) acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%modal-spec modal rows inner)
      (let ((mode (%ref modal 'mode 'new-chain)) (err (%ref modal 'error #f)))
        (cond
         ((eq? mode 'new-chain)
          (list "new all-in-one chain"
            (list (cons #f (string-append "id: " (%ref modal 'input "") "_"))
              (cons #f
                (if err
                  (string-append "error: " err)
                  (string-append %dim "letters, digits, -, _" %reset)
                ) ;if
              ) ;cons
            ) ;list
            " Enter create · Esc cancel"
          ) ;list
         ) ;
         ((eq? mode 'pick-provider)
          (let ((targets (%ref modal 'targets '())) (p-idx (%ref modal 'p-idx 0)))
            (list (string-append "add slot to " (%ref modal 'chain-id "") " · step 1/2 provider")
              (if (null? targets)
                (list (cons #f "no enabled providers/models"))
                (%render-items targets
                  (%clamp p-idx (length targets))
                  rows
                  (lambda (entry)
                    (string-append "● "
                      (car entry)
                      " ("
                      (number->string (length (cdr entry)))
                      " models)"
                    ) ;string-append
                  ) ;lambda
                  inner
                ) ;%render-items
              ) ;if
              " ↑↓/jk move · Enter/→ models · Esc cancel"
            ) ;list
          ) ;let
         ) ;
         ((eq? mode 'pick-model)
          (let* ((targets (%ref modal 'targets '()))
                 (p-entry (list-ref targets (%clamp (%ref modal 'p-idx 0) (length targets))))
                 (p-id (car p-entry))
                 (models (cdr p-entry))
                 (m-idx (%clamp (%ref modal 'm-idx 0) (length models)))
                ) ;
            (list (string-append "add slot · step 2/2 model (" p-id ")")
              (%render-items models
                m-idx
                rows
                (lambda (m)
                  (string-append "● "
                    (pi-config-model-id m)
                    (if (pi-config-model-reasoning? m) " (thinking)" "")
                  ) ;string-append
                ) ;lambda
                inner
              ) ;%render-items
              " ↑↓/jk move · Enter next/add · ←/Esc back · q cancel"
            ) ;list
          ) ;let*
         ) ;
         (else
           (let ((t-idx (%clamp (%ref modal 't-idx 0) (length pi-thinking-levels)))
                 (sub (if (eq? mode 'pick-thinking) "add slot · thinking level" "set thinking level")
                 ) ;sub
                ) ;
             (list sub
               (%render-items pi-thinking-levels
                 t-idx
                 rows
                 (lambda (lvl) (string-append "● " lvl))
                 inner
               ) ;%render-items
               " ↑↓/jk move · Enter confirm · ←/Esc back/cancel"
             ) ;list
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-config-aio-modal-render modal width height)
      (let* ((ow (max 24 (min (- width 6) 72)))
             (inner (- ow 4))
             (oh (max 6 (min 14 (- height 4))))
             (rows (max 1 (- oh 3)))
             (spec (%modal-spec modal rows inner))
             (title (car spec))
             (raw-items (%pad (cadr spec) rows))
             (hint (caddr spec))
             (pad-x (max 0 (quotient (- width ow) 2)))
             (pad-y (max 0 (quotient (- height oh) 2)))
             (head (string-append %accent
                     "┌"
                     (%fit (string-append " " title) (+ inner 2))
                     "┐"
                     %reset
                   ) ;string-append
             ) ;head
             (foot (string-append %accent "└" (utf8-make-string (+ inner 2) #\─) "┘" %reset)
             ) ;foot
             (body
               (map
                 (lambda (item)
                   (if (pair? item)
                     (let ((cell (string-append " " (%fit (cdr item) inner) " ")))
                       (if (car item)
                         (string-append %accent "│" %reset %selected-style
                           cell %reset %accent "│" %reset
                         ) ;string-append
                         (string-append %accent "│" %reset cell %accent "│" %reset)
                       ) ;if
                     ) ;let
                     (string-append %accent
                       "│"
                       %reset
                       " "
                       (%fit "" inner)
                       " "
                       %accent
                       "│"
                       %reset
                     ) ;string-append
                   ) ;if
                 ) ;lambda
                 raw-items
               ) ;map
             ) ;body
             (box (append (list head) body (list foot (%fit hint ow))))
             (blank (%fit "" width))
             (raw-lines
               (append (map (lambda (i) blank) (iota pad-y))
                 (map (lambda (line) (string-append (%fit "" pad-x) line (%fit "" pad-x))) box)
                 (map (lambda (i) blank) (iota (max 0 (- height pad-y oh))))
               ) ;append
             ) ;raw-lines
             (len (length raw-lines))
            ) ;
        (cond ((<= height 0) '())
              ((> len height) (take raw-lines height))
              ((< len height) (append raw-lines (make-list (- height len) blank)))
              (else raw-lines)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
