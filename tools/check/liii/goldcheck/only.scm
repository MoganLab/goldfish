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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldcheck only)
  (import (scheme base) (liii base) (liii list) (liii json))
  (export restrict-check-config single-rule-config)
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

    ;; --only 模式的 cfg：只启用指定规则，excludes 保持生效
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
                     (if (string=? (car kv) only-name)
                       (%with-enabled (cdr kv) #t)
                       (%with-enabled (cdr kv) #f)
                     ) ;if
                   ) ;cons
                 ) ;lambda
                 rules
               ) ;map
             ) ;new-rules
             (excludes
               (let ((ex (json-ref cfg "excludes")))
                 (if (pair? ex) ex '())
               ) ;let
             ) ;excludes
             (top
               (filter
                 (lambda (kv)
                   (and (pair? kv)
                     (not (string=? (car kv) "rules"))
                     (not (string=? (car kv) "excludes"))
                   ) ;and
                 ) ;lambda
                 (if (pair? cfg) cfg '())
               ) ;filter
             ) ;top
            ) ;
        (cons (cons "rules" new-rules)
          (if (null? excludes) top (cons (cons "excludes" excludes) top))
        ) ;cons
      ) ;let*
    ) ;define

    (define (single-rule-config cfg rule-name all-rule-names)
      "构造仅启用指定规则的新配置，保留该规则自身的 exclude 等字段。"
      (let ((rules (if cfg (json-ref cfg "rules") '())))
        (let ((new-rules
                (map
                  (lambda (name)
                    (let ((rule (if (json-object? rules) (json-ref rules name) '())))
                      (cons name (%with-enabled rule (string=? name rule-name)))
                    ) ;let
                  ) ;lambda
                  all-rule-names
                ) ;map
              ) ;new-rules
             ) ;
          (if cfg
            (cons (cons "rules" new-rules)
              (filter
                (lambda (kv) (not (string=? (car kv) "rules")))
                cfg
              ) ;filter
            ) ;cons
            (list (cons "rules" new-rules))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
