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

(define-library (liii goldcheck report)
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii base)
    (liii string)
    (liii goldcheck rules)
  ) ;import
  (export format-rule-line display-rule-result violation-report-lines
    violation-display-hint report-and-exit
  ) ;export
  (begin

    (define (format-rule-line ok name elapsed)
      (string-append "  ["
        (if ok "OK    " "FAILED")
        "] "
        name
        (make-string (max 1 (- 17 (string-length name))) #\space)
        (format #f "~,3fs" elapsed)
      ) ;string-append
    ) ;define

    (define (display-rule-result result)
      (display (format-rule-line (cadr result) (car result) (caddr result)))
      (newline)
    ) ;define

    (define (%more-line rule n)
      (string-append "  ... and "
        (number->string n)
        " more "
        (if (symbol? rule) (symbol->string rule) (object->string rule))
        " violation(s)"
      ) ;string-append
    ) ;define

    ;; cap 下实际展示的违规条数（不含汇总行）
    (define (%count-shown violations cap)
      (let ((total (vector-length violations)))
        (let loop
          ((i 0) (rule #f) (n 0) (acc 0))
          (if (>= i total)
            acc
            (let ((r (car (vector-ref violations i))))
              (if (eq? r rule)
                (if (or (not cap) (< n cap))
                  (loop (+ i 1) r (+ n 1) (+ acc 1))
                  (loop (+ i 1) r (+ n 1) acc)
                ) ;if
                (loop (+ i 1) r 1 (+ acc 1))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (violation-report-lines violations cap)
      "将违规向量转为展示行：cap 为正整数时同一规则最多展示 cap 条，\n超出部分汇总为一行提示；cap 为 #f 时不封顶全部展示。"
      (let ((total (vector-length violations)))
        (let loop
          ((i 0) (cur-rule #f) (shown 0) (hidden 0) (acc '()))
          (if (>= i total)
            (reverse (if (> hidden 0) (cons (%more-line cur-rule hidden) acc) acc))
            (let* ((v (vector-ref violations i)) (rule (car v)))
              (if (eq? rule cur-rule)
                (if (or (not cap) (< shown cap))
                  (loop (+ i 1) rule (+ shown 1) hidden (cons (format-violation v) acc))
                  (loop (+ i 1) rule shown (+ hidden 1) acc)
                ) ;if
                (loop (+ i 1)
                  rule
                  1
                  0
                  (cons (format-violation v)
                    (if (> hidden 0) (cons (%more-line cur-rule hidden) acc) acc)
                  ) ;cons
                ) ;loop
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (violation-display-hint violations cap)
      "生成封顶截断时的提示行；未截断或未封顶（cap 为 #f）时返回 #f。"
      (if (not cap)
        #f
        (let* ((total (vector-length violations)) (shown (%count-shown violations cap)))
          (if (>= shown total)
            #f
            (string-append "Only "
              (number->string shown)
              " of "
              (number->string total)
              " violation(s) displayed; run 'gf check --only=<rule>' to show the full list."
            ) ;string-append
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (report-and-exit violations total-elapsed . maybe-cap)
      (let ((cap (if (null? maybe-cap) #f (car maybe-cap))))
        (display (format-rule-line (zero? (vector-length violations)) "all" total-elapsed)
        ) ;display
        (newline)
        (if (zero? (vector-length violations))
          (exit 0)
          (begin
            (display (string-append "Check failed: "
                       (number->string (vector-length violations))
                       " violation(s) found"
                     ) ;string-append
            ) ;display
            (newline)
            (for-each (lambda (line) (display line) (newline))
              (violation-report-lines violations cap)
            ) ;for-each
            (let ((hint (violation-display-hint violations cap)))
              (when hint
                (display hint)
                (newline)
              ) ;when
            ) ;let
            (exit 1)
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
