;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (only (scheme base) let-values)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

(let ((queue (make-list-queue '(1 2 3))))
  (check-true (list-queue? queue))
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-front queue) => 1)
  (check (list-queue-back queue) => 3)
) ;let

(let* ((pairs (list 'a 'b 'c))
       (queue (make-list-queue pairs (cddr pairs)))
) ;let*
  (let-values (((first last) (list-queue-first-last queue)))
    (check-true (eq? first pairs))
    (check-true (eq? last (cddr pairs)))
  ) ;let-values
  (list-queue-add-back! queue 'd)
  (check pairs => '(a b c d))
  (check (list-queue-list queue) => '(a b c d))
) ;let*

(let* ((queue (list-queue 1 2 3))
       (copy (list-queue-copy queue))
) ;let*
  (list-queue-add-front! copy 0)
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-list copy) => '(0 1 2 3))
) ;let*

(let ((queue (list-queue)))
  (check-true (list-queue-empty? queue))
  (list-queue-add-front! queue 2)
  (list-queue-add-front! queue 1)
  (list-queue-add-back! queue 3)
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-remove-front! queue) => 1)
  (check (list-queue-remove-back! queue) => 3)
  (check (list-queue-remove-front! queue) => 2)
  (check-true (list-queue-empty? queue))
  (check-catch 'out-of-range (list-queue-front queue))
  (check-catch 'out-of-range (list-queue-back queue))
  (check-catch 'out-of-range (list-queue-remove-front! queue))
  (check-catch 'out-of-range (list-queue-remove-back! queue))
) ;let

(let ((queue (list-queue 1 2 3)))
  (check (list-queue-remove-all! queue) => '(1 2 3))
  (check-true (list-queue-empty? queue))
) ;let

(let* ((queue-1 (list-queue 1 2))
       (queue-2 (list-queue 3 4))
       (queue-3 (list-queue-append queue-1 queue-2))
) ;let*
  (check (list-queue-list queue-3) => '(1 2 3 4))
  (check (list-queue-list queue-1) => '(1 2))
  (check (list-queue-list queue-2) => '(3 4))
) ;let*

(let ((queue (list-queue-append! (list-queue) (list-queue 1 2) (list-queue 3 4))))
  (check (list-queue-list queue) => '(1 2 3 4))
) ;let

(let ((queue (list-queue-concatenate
              (list (list-queue 1)
                    (list-queue)
                    (list-queue 2 3)))
              ) ;list
) ;let
  (check (list-queue-list queue) => '(1 2 3))
) ;let

(let* ((queue (list-queue 1 2 3))
       (mapped (list-queue-map (lambda (x) (* x 10)) queue))
       (sum 0)
) ;let*
  (check (list-queue-list mapped) => '(10 20 30))
  (list-queue-map! (lambda (x) (+ x 1)) queue)
  (check (list-queue-list queue) => '(2 3 4))
  (list-queue-for-each (lambda (x) (set! sum (+ sum x))) mapped)
  (check sum => 60)
) ;let*

(let* ((pairs (list 5 6 7))
       (last (cddr pairs))
       (queue (list-queue 1 2))
) ;let*
  (list-queue-set-list! queue pairs last)
  (check (list-queue-list queue) => '(5 6 7))
  (list-queue-add-back! queue 8)
  (check pairs => '(5 6 7 8))
) ;let*

(let ((queue (list-queue 9 10)))
  (list-queue-set-list! queue '(4 5))
  (check (list-queue-list queue) => '(4 5))
) ;let

(let ((queue (list-queue-unfold (lambda (x) (> x 3))
                                (lambda (x) (* x 2))
                                (lambda (x) (+ x 1))
                                0)))
  (check (list-queue-list queue) => '(0 2 4 6))
) ;let

(let ((queue (list-queue-unfold-right (lambda (x) (> x 3))
                                      (lambda (x) (* x 2))
                                      (lambda (x) (+ x 1))
                                      0)))
  (check (list-queue-list queue) => '(6 4 2 0))
) ;let

(let ((queue (list-queue-unfold (lambda (x) (> x 3))
                                (lambda (x) (* x 2))
                                (lambda (x) (+ x 1))
                                0
                                (list-queue 8))))
  (check (list-queue-list queue) => '(0 2 4 6 8))
) ;let

(let ((queue (list-queue-unfold-right (lambda (x) (> x 3))
                                      (lambda (x) (* x 2))
                                      (lambda (x) (+ x 1))
                                      0
                                      (list-queue 8))))
  (check (list-queue-list queue) => '(8 6 4 2 0))
) ;let

(check-report)
