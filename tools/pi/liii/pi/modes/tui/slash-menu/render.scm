;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui slash-menu render)
  (import (scheme base)
    (liii error)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii tui box)
    (liii pi modes tui slash-menu types)
  ) ;import
  (export render-slash-menu overlay-slash-menu-lines)
  (begin

    (define (%pad-right-str str target-w)
      (let ((w (display-width str)))
        (if (>= w target-w)
          str
          (string-append str (make-string (- target-w w) #\space))
        ) ;if
      ) ;let
    ) ;define

    (define (render-slash-menu menu width . maybe-max-height)
      (unless (and (exact-integer? width) (> width 0))
        (type-error "render-slash-menu: expected positive integer width")
      ) ;unless
      (let* ((max-h (if (pair? maybe-max-height) (car maybe-max-height) #f))
             (cands (slash-menu-candidates menu))
             (sel-idx (slash-menu-selected-index menu))
             (inner-w (max 1 (- width 2)))
             (max-items (if (and max-h (>= max-h 3)) (min 6 (- max-h 2)) 5))
            ) ;
        (if (null? cands)
          (let ((box (make-box " 无匹配命令" :style 'single :title "Commands")))
            (component-render box width)
          ) ;let
          (let* ((total (length cands))
                 (start
                   (max 0 (min (- total max-items) (max 0 (- sel-idx (floor (/ max-items 2))))))
                 ) ;start
                 (end (min total (+ start max-items)))
                 (sliced-indices
                   (let loop
                     ((i start) (acc '()))
                     (if (>= i end) (reverse acc) (loop (+ i 1) (cons i acc)))
                   ) ;let
                 ) ;sliced-indices
                 (item-lines
                   (map
                     (lambda (i)
                       (let* ((cmd (list-ref cands i))
                              (prefix (if (= i sel-idx) "> " "  "))
                              (name (%pad-right-str (slash-command-name cmd) 12))
                              (desc (slash-command-description cmd))
                              (raw-line (string-append prefix name " " desc))
                             ) ;
                         (truncate-to-width raw-line inner-w)
                       ) ;let*
                     ) ;lambda
                     sliced-indices
                   ) ;map
                 ) ;item-lines
                 (box (make-box (apply make-vstack item-lines) :style 'single :title "Commands"))
                ) ;
            (component-render box width)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (overlay-slash-menu-lines menu msg-lines width height)
      (if (<= height 0)
        '()
        (let* ((rendered (render-slash-menu menu width (min 8 height)))
               (menu-h (length rendered))
              ) ;
          (if (>= menu-h height)
            (let ((drop-cnt (- menu-h height)))
              (let loop
                ((rest rendered) (i 0))
                (if (>= i drop-cnt) rest (loop (cdr rest) (+ i 1)))
              ) ;let
            ) ;let
            (let* ((keep-h (- height menu-h))
                   (top-part
                     (let loop
                       ((rest msg-lines) (i 0) (acc '()))
                       (if (or (null? rest) (>= i keep-h))
                         (reverse acc)
                         (loop (cdr rest) (+ i 1) (cons (car rest) acc))
                       ) ;if
                     ) ;let
                   ) ;top-part
                   (padded-top
                     (let ((cur-len (length top-part)))
                       (if (>= cur-len keep-h)
                         top-part
                         (append top-part (make-list (- keep-h cur-len) (make-string width #\space)))
                       ) ;if
                     ) ;let
                   ) ;padded-top
                  ) ;
              (append padded-top rendered)
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
