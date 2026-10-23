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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii goldcheck only)
  (import (scheme base) (liii base) (liii list) (liii json))
  (export restrict-check-config)
  (begin

    ;; 规则对象改写 enabled
    (define (%with-enabled rule enabled)
      (cons (cons "enabled" enabled)
        (filter
          (lambda (kv) (not (string=? (car kv) "enabled")))
          (if (pair? rule) rule '())
        ) ;filter
      ) ;cons
    ) ;define

    ;; --only 模式的 cfg：只启用指定规则，并去掉 exclude 豁免
    (define (restrict-check-config cfg only-name)
      (let* ((rules
               (let ((r (json-ref cfg "rules")))
                 (if (pair? r) r '())
               ) ;let
             ) ;rules
             (new-rules
               (map
                 (lambda (kv)
                   (cons (car kv)
                     (cond
                      ((string=? (car kv) only-name)
                       (if (and (string=? only-name "layer-order") (pair? (cdr kv)))
                         (cdr kv)
                         (%with-enabled (cdr kv) #t)
                       ) ;if
                      ) ;
                      ((string=? (car kv) "layer-order") (list (cons "enabled" #f)))
                      (else (%with-enabled (cdr kv) #f))
                     ) ;cond
                   ) ;cons
                 ) ;lambda
                 rules
               ) ;map
             ) ;new-rules
             (top
               (filter
                 (lambda (kv) (and (pair? kv) (not (string=? (car kv) "exclude"))))
                 (if (pair? cfg) cfg '())
               ) ;filter
             ) ;top
            ) ;
        (cons (cons "rules" new-rules) top)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
