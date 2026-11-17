;;
;; Copyright (C) 2026 Liii Network
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii cmark stem lists)
  (import (scheme base))
  (import (liii cmark block list))
  (export make-bullet-list-stem)
  (export make-ordered-list-stem)
  (export make-list-item-stem)
  (begin
    (define (list-item-task-status lines)
      (and (pair? lines) (task-list-item-status (car lines)))
    ) ;define

    (define (strip-task-list-item-lines lines)
      (if (null? lines)
        '()
        (cons (strip-task-list-item-marker (car lines)) (cdr lines))
      ) ;if
    ) ;define

    (define (make-bullet-list-stem tightness item-lines-list parse-blocks)
      (cons 'bullet-list
        (cons tightness
          (map (lambda (lines) (make-list-item-stem lines parse-blocks)) item-lines-list)
        ) ;cons
      ) ;cons
    ) ;define

    (define (make-ordered-list-stem start delimiter tightness item-lines-list parse-blocks)
      (cons 'ordered-list
        (cons start
          (cons delimiter
            (cons tightness
              (map (lambda (lines) (make-list-item-stem lines parse-blocks)) item-lines-list)
            ) ;cons
          ) ;cons
        ) ;cons
      ) ;cons
    ) ;define

    (define (make-list-item-stem lines parse-blocks)
      (let ((task-status (list-item-task-status lines)))
        (if task-status
          (cons 'item
            (cons task-status (parse-blocks (strip-task-list-item-lines lines)))
          ) ;cons
          (cons 'item (parse-blocks lines))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
