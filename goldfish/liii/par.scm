;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii par)
  (import (scheme base) (liii error) (liii go))
  (export par-for-each par-map)
  (begin
    (define (par-for-each f l)
      (unless (procedure? f)
        (type-error "par-for-each: first argument must be a procedure" f)
      ) ;unless
      (unless (list? l)
        (type-error "par-for-each: second argument must be a list" l)
      ) ;unless
      (unless (null? l)
        (let* ((n (length l)) (done-ch (make-chan n)))
          (for-each (lambda (elem) (go-apply f (list elem) done-ch)) l)
          ;; Join Barrier：收满 n 个结果，全部完成后重抛首个异常
          (let loop
            ((i 0) (first-error #f))
            (if (= i n)
              (when first-error
                (apply error (cadr first-error) (caddr first-error))
              ) ;when
              (let ((res (chan-recv! done-ch)))
                (if (and (pair? res) (eq? (car res) 'error))
                  (loop (+ i 1) (or first-error res))
                  (loop (+ i 1) first-error)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;unless
    ) ;define

    (define (par-map f l)
      (unless (procedure? f)
        (type-error "par-map: first argument must be a procedure" f)
      ) ;unless
      (unless (list? l)
        (type-error "par-map: second argument must be a list" l)
      ) ;unless
      (if (null? l)
        '()
        (let* ((chans (map (lambda (_) (make-chan 1)) l)))
          (for-each (lambda (elem ch) (go-apply f (list elem) ch)) l chans)
          ;; Join Barrier：按序读取每个专属通道，收满所有结果
          (let loop
            ((chs chans) (results '()) (first-error #f))
            (if (null? chs)
              (if first-error
                (apply error (cadr first-error) (caddr first-error))
                (reverse results)
              ) ;if
              (let ((res (chan-recv! (car chs))))
                (if (and (pair? res) (eq? (car res) 'error))
                  (loop (cdr chs) results (or first-error res))
                  (loop (cdr chs) (cons (cadr res) results) first-error)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
