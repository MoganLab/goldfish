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

(define-library (liii tui editor history)
  (export make-history history? history-entries history-max-size history-index
    history-draft history-push! history-prev history-next history-reset!
  ) ;export
  (import (scheme base) (scheme write))
  (begin

    (define-record-type <history>
      (%make-history-record entries max-size index draft)
      history?
      (entries %history-entries %history-entries-set!)
      (max-size %history-max-size %history-max-size-set!)
      (index %history-index %history-index-set!)
      (draft %history-draft %history-draft-set!)
    ) ;define-record-type

    (define (make-history . args)
      (let ((max-sz (if (null? args) 100 (car args))))
        (%make-history-record '() max-sz #f #f)
      ) ;let
    ) ;define

    (define (history-entries h)
      (%history-entries h)
    ) ;define

    (define (history-max-size h)
      (%history-max-size h)
    ) ;define

    (define (history-index h)
      (%history-index h)
    ) ;define

    (define (history-draft h)
      (%history-draft h)
    ) ;define

    (define (history-push! h text)
      ;; 忽略空字符串
      (unless (or (not (string? text)) (string=? text ""))
        (let ((ents (%history-entries h)))
          ;; 去重：如果与上一条相同则不重复记录
          (unless (and (pair? ents) (string=? (car ents) text))
            (let* ((new-ents (cons text ents))
                   (max-sz (%history-max-size h))
                   (trimmed
                     (if (> (length new-ents) max-sz)
                       (let loop
                         ((lst new-ents) (n max-sz))
                         (if (<= n 0) '() (cons (car lst) (loop (cdr lst) (- n 1))))
                       ) ;let
                       new-ents
                     ) ;if
                   ) ;trimmed
                  ) ;
              (%history-entries-set! h trimmed)
            ) ;let*
          ) ;unless
        ) ;let
      ) ;unless
      (history-reset! h)
    ) ;define

    (define (history-reset! h)
      (%history-index-set! h #f)
      (%history-draft-set! h #f)
    ) ;define

    (define (history-prev h current-text)
      (let ((ents (%history-entries h)) (idx (%history-index h)))
        (cond ((null? ents) #f)
              ((not idx)
               ;; 第一次向上按：保存当前输入为 draft，展示 entries[0]
               (%history-draft-set! h current-text)
               (%history-index-set! h 0)
               (car ents)
              ) ;
              ((< idx (- (length ents) 1))
               (let ((new-idx (+ idx 1)))
                 (%history-index-set! h new-idx)
                 (list-ref ents new-idx)
               ) ;let
              ) ;
              (else
                ;; 已经到达最顶部历史，返回当前最顶条目
                (list-ref ents idx)
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (history-next h current-text)
      (let ((ents (%history-entries h)) (idx (%history-index h)))
        (cond ((not idx) #f)
              ((> idx 0)
               (let ((new-idx (- idx 1)))
                 (%history-index-set! h new-idx)
                 (list-ref ents new-idx)
               ) ;let
              ) ;
              ((= idx 0)
               ;; 恢复草稿并退出历史浏览
               (let ((draft (or (%history-draft h) "")))
                 (%history-index-set! h #f)
                 (%history-draft-set! h #f)
                 draft
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
