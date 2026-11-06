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

(define-library (liii tui select-list)
  (export make-select-list select-list? select-list-items
    select-list-selected-item select-list-selected-index
    select-list-set-selected-index!
  ) ;export
  (import (scheme base)
    (liii string)
    (liii list)
    (liii tui component)
    (liii tui text-width)
  ) ;import
  (begin

    (define-record-type <select-list-state>
      (%make-select-list-state items selected max-visible on-select on-cancel
        selected-text description-text scroll-text no-match-text min-primary
        max-primary
      ) ;%make-select-list-state
      select-list-state?
      (items select-list-state-items)
      (selected select-list-state-selected select-list-state-selected-set!)
      (max-visible select-list-state-max-visible)
      (on-select select-list-state-on-select)
      (on-cancel select-list-state-on-cancel)
      (selected-text select-list-state-selected-text)
      (description-text select-list-state-description-text)
      (scroll-text select-list-state-scroll-text)
      (no-match-text select-list-state-no-match-text)
      (min-primary select-list-state-min-primary)
      (max-primary select-list-state-max-primary)
    ) ;define-record-type

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%state comp)
      (let ((data (component-data comp)))
        (and (select-list-state? data) data)
      ) ;let
    ) ;define

    (define (select-list? obj)
      (and (component? obj) (select-list-state? (%state obj)))
    ) ;define

    (define (select-list-items comp)
      (select-list-state-items (%state comp))
    ) ;define

    (define (select-list-selected-index comp)
      (select-list-state-selected (%state comp))
    ) ;define

    (define (select-list-selected-item comp)
      (let* ((state (%state comp)) (items (select-list-state-items state)))
        (and (pair? items) (list-ref items (select-list-state-selected state)))
      ) ;let*
    ) ;define

    (define (select-list-set-selected-index! comp index)
      (let* ((state (%state comp)) (count (length (select-list-state-items state))))
        (select-list-state-selected-set! state
          (if (zero? count) 0 (max 0 (min index (- count 1))))
        ) ;select-list-state-selected-set!
        comp
      ) ;let*
    ) ;define

    (define (%item-value item key default)
      (let ((pair (assoc key item)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%display-value item)
      (%item-value item 'label (%item-value item 'value ""))
    ) ;define

    (define (%normalize-description value)
      (string-trim-both (string-replace (string-replace (or value "") "\r" " ") "\n" " ")
      ) ;string-trim-both
    ) ;define

    (define (%primary-width state)
      (let* ((min-w (select-list-state-min-primary state))
             (max-w (select-list-state-max-primary state))
             (widest
               (fold
                 (lambda (item widest) (max widest (+ (display-width (%display-value item)) 2)))
                 0
                 (select-list-state-items state)
               ) ;fold
             ) ;widest
            ) ;
        (max min-w (min max-w widest))
      ) ;let*
    ) ;define

    (define (%visible-range state)
      (let* ((items (select-list-state-items state))
             (count (length items))
             (max-visible (select-list-state-max-visible state))
             (selected (select-list-state-selected state))
             (start
               (max 0 (min (- selected (quotient max-visible 2)) (- count max-visible)))
             ) ;start
            ) ;
        (cons start (min (+ start max-visible) count))
      ) ;let*
    ) ;define

    (define (%render-item state item selected? width primary-width)
      (let* ((prefix (if selected? "→ " "  "))
             (description (%item-value item 'description #f))
             (single-description (and description (%normalize-description description)))
            ) ;
        (if (and single-description (> width 40))
          (let* ((effective (max 1 (min primary-width (- width 2 4))))
                 (value (truncate-to-width (%display-value item) (max 1 (- effective 2)) ""))
                 (spacing
                   (make-string (max 1 (- effective (display-width value))) #\space)
                 ) ;spacing
                 (description-start (+ 2 (display-width value) (string-length spacing)))
                 (remaining (- width description-start 2))
                ) ;
            (if (> remaining 10)
              (let ((desc (truncate-to-width single-description remaining "")))
                (if selected?
                 ((select-list-state-selected-text state)
                  (string-append prefix value spacing desc)
                 ) ;
                 (string-append prefix
                   value
                   ((select-list-state-description-text state) (string-append spacing desc))
                 ) ;string-append
                ) ;if
              ) ;let
              ((if selected? (select-list-state-selected-text state) (lambda (x) x))
               (string-append prefix
                 (truncate-to-width (%display-value item) (max 0 (- width 4)) "")
               ) ;string-append
              ) ;
            ) ;if
          ) ;let*
          ((if selected? (select-list-state-selected-text state) (lambda (x) x))
           (string-append prefix
             (truncate-to-width (%display-value item) (max 0 (- width 4)) "")
           ) ;string-append
          ) ;
        ) ;if
      ) ;let*
    ) ;define

    (define (%render comp width)
      (let* ((state (%state comp)) (items (select-list-state-items state)))
        (if (null? items)
          (list ((select-list-state-no-match-text state) "  No matching commands"))
          (let* ((range (%visible-range state))
                 (start (car range))
                 (end (cdr range))
                 (primary (%primary-width state))
                 (rows
                   (map
                     (lambda (item index)
                       (%render-item state
                         item
                         (= index (select-list-state-selected state))
                         width
                         primary
                       ) ;%render-item
                     ) ;lambda
                     (list-tail items start)
                     (iota (- end start) start)
                   ) ;map
                 ) ;rows
                 (visible-rows (take rows (- end start)))
                ) ;
            (if (or (> start 0) (< end (length items)))
              (append visible-rows
                (list
                 ((select-list-state-scroll-text state)
                  (truncate-to-width
                    (string-append "  ("
                      (number->string (+ (select-list-state-selected state) 1))
                      "/"
                      (number->string (length items))
                      ")"
                    ) ;string-append
                    (max 0 (- width 2))
                    ""
                  ) ;truncate-to-width
                 ) ;
                ) ;list
              ) ;append
              visible-rows
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%handle-input comp key)
      (let* ((state (%state comp)) (count (length (select-list-state-items state))))
        (cond
         ((and (eq? key 'up) (> count 0))
          (select-list-state-selected-set! state
            (modulo (- (select-list-state-selected state) 1) count)
          ) ;select-list-state-selected-set!
          #t
         ) ;
         ((and (eq? key 'down) (> count 0))
          (select-list-state-selected-set! state
            (modulo (+ (select-list-state-selected state) 1) count)
          ) ;select-list-state-selected-set!
          #t
         ) ;
         ((eq? key 'enter)
          (let ((item (select-list-selected-item comp))
                (callback (select-list-state-on-select state))
               ) ;
            (when (and item (procedure? callback))
              (callback item)
            ) ;when
          ) ;let
          #t
         ) ;
         ((or (eq? key 'escape) (eq? key 'ctrl-c))
          (let ((callback (select-list-state-on-cancel state)))
            (when (procedure? callback)
              (callback)
            ) ;when
          ) ;let
          #t
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (make-select-list items max-visible . args)
      (let* ((raw-min (%keyword-value :min-primary-column-width args 32))
             (raw-max (%keyword-value :max-primary-column-width args 32))
             (state
               (%make-select-list-state items
                 0
                 (max 1 max-visible)
                 (%keyword-value :on-select args #f)
                 (%keyword-value :on-cancel args #f)
                 (%keyword-value :selected-text args (lambda (x) x))
                 (%keyword-value :description-text args (lambda (x) x))
                 (%keyword-value :scroll-text args (lambda (x) x))
                 (%keyword-value :no-match-text args (lambda (x) x))
                 (max 1 (min raw-min raw-max))
                 (max 1 (max raw-min raw-max))
               ) ;%make-select-list-state
             ) ;state
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (component-data-set! comp state)
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
