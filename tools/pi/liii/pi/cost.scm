;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi cost)
  (import (scheme base) (liii string) (liii list) (liii pi models builtin))
  (export pi-model-cost pi-calculate-cost pi-calculate-cache-savings
    pi-summarize-session-cost PI_DEFAULT_MODEL_COSTS
  ) ;export
  (begin

    (define PI_DEFAULT_MODEL_COSTS
      '(("google"
         (("id" . "gemini-3.8-flash")
          ("cost"
           (input . 0.75)
           (output . 3.75)
           (cacheRead . 0.075)
           (cacheWrite . 0.0)))
         (("id" . "gemini-2.5-flash")
          ("cost"
           (input . 0.3)
           (output . 2.5)
           (cacheRead . 0.03)
           (cacheWrite . 0.0)))
         (("id" . "gemini-2.5-pro")
          ("cost"
           (input . 1.25)
           (output . 10.0)
           (cacheRead . 0.125)
           (cacheWrite . 0.0)
           (tiers ((inputTokensAbove . 200000)
                   (input . 2.5)
                   (output . 15.0)
                   (cacheRead . 0.25)
                   (cacheWrite . 0.0))))))
        ("anthropic"
         (("id" . "claude-3-7-sonnet-20250219")
          ("cost"
           (input . 3.0)
           (output . 15.0)
           (cacheRead . 0.3)
           (cacheWrite . 3.75)))
         (("id" . "claude-3-5-sonnet-20241022")
          ("cost"
           (input . 3.0)
           (output . 15.0)
           (cacheRead . 0.3)
           (cacheWrite . 3.75))))
        ("kimi-coding"
         (("id" . "kimi-for-coding")
          ("cost"
           (input . 0.95)
           (output . 4.0)
           (cacheRead . 0.19)
           (cacheWrite . 0.0)))
         (("id" . "kimi-for-coding-highspeed")
          ("cost"
           (input . 1.9)
           (output . 8.0)
           (cacheRead . 0.38)
           (cacheWrite . 0.0)))
         (("id" . "k3")
          ("cost"
           (input . 3.0)
           (output . 15.0)
           (cacheRead . 0.3)
           (cacheWrite . 0.0))))
        ("glm"
         (("id" . "glm-4-plus")
          ("cost"
           (input . 1.4)
           (output . 1.4)
           (cacheRead . 0.7)
           (cacheWrite . 0.0))))
        ("deepseek"
         (("id" . "deepseek-chat")
          ("cost"
           (input . 0.27)
           (output . 1.1)
           (cacheRead . 0.07)
           (cacheWrite . 0.0)))
         (("id" . "deepseek-reasoner")
          ("cost"
           (input . 0.55)
           (output . 2.19)
           (cacheRead . 0.14)
           (cacheWrite . 0.0))))
        ("minimax"
         (("id" . "MiniMax-M3.1-Flash")
          ("cost"
           (input . 0.3)
           (output . 1.2)
           (cacheRead . 0.06)
           (cacheWrite . 0.0)))
         (("id" . "MiniMax-M3")
          ("cost"
           (input . 0.3)
           (output . 1.2)
           (cacheRead . 0.06)
           (cacheWrite . 0.0)
           (tiers ((inputTokensAbove . 512000)
                   (input . 0.6)
                   (output . 2.4)
                   (cacheRead . 0.12)
                   (cacheWrite . 0.0)))))
         (("id" . "MiniMax-M2.7")
          ("cost"
           (input . 0.3)
           (output . 1.2)
           (cacheRead . 0.06)
           (cacheWrite . 0.375)))
         (("id" . "MiniMax-M2.7-highspeed")
          ("cost"
           (input . 0.6)
           (output . 2.4)
           (cacheRead . 0.06)
           (cacheWrite . 0.375)))))
    ) ;define

    (define (%safe-assoc key alist)
      (and (pair? alist) (every pair? alist) (assoc key alist))
    ) ;define

    (define (%val key alist default-val)
      (if (not (pair? alist))
        default-val
        (let* ((str-k (if (symbol? key) (symbol->string key) key))
               (sym-k (if (string? key) (string->symbol key) key))
               (p (or (%safe-assoc key alist) (%safe-assoc str-k alist) (%safe-assoc sym-k alist))
               ) ;p
              ) ;
          (if (pair? p) (cdr p) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%find-model-cost-in-store models-store provider model-id)
      (and (pair? models-store)
        (let* ((p-data (%val provider models-store '()))
               (models (%val "models" p-data '()))
               (m-list (cond ((vector? models) (vector->list models))
                             ((list? models) models)
                             (else '())
                       ) ;cond
               ) ;m-list
              ) ;
          (let ((m (find (lambda (m) (string=? (%val "id" m "") model-id)) m-list)))
            (and m
              (let ((c (%val "cost" m #f)))
                (and (pair? c) c)
              ) ;let
            ) ;and
          ) ;let
        ) ;let*
      ) ;and
    ) ;define

    (define (%find-model-cost-builtin provider model-id)
      (let* ((p-data (%val provider PI_DEFAULT_MODEL_COSTS '()))
             (match
               (and (pair? p-data)
                 (find (lambda (m) (string=? (%val "id" m "") model-id)) p-data)
               ) ;and
             ) ;match
            ) ;
        (and match (%val "cost" match #f))
      ) ;let*
    ) ;define

    (define (%minimax-alias provider)
      (and (string? provider) (string=? provider "minimax-cn") "minimax")
    ) ;define

    (define (pi-model-cost provider model-id models-store)
      (let ((fallback-p (%minimax-alias provider)))
        (or (%find-model-cost-in-store models-store provider model-id)
          (and fallback-p (%find-model-cost-in-store models-store fallback-p model-id))
          (%find-model-cost-builtin provider model-id)
          (and fallback-p (%find-model-cost-builtin fallback-p model-id))
          '((input . 0.0) (output . 0.0) (cacheRead . 0.0) (cacheWrite . 0.0))
        ) ;or
      ) ;let
    ) ;define

    (define (%resolve-cost-tier cost-rates total-input-tokens)
      (let ((tiers (%val 'tiers cost-rates '())))
        (if (not (pair? tiers))
          cost-rates
          (let loop
            ((t-list tiers) (best cost-rates) (max-thresh -1))
            (if (null? t-list)
              best
              (let* ((tier (car t-list)) (thresh (%val 'inputTokensAbove tier -1)))
                (if (and (> total-input-tokens thresh) (> thresh max-thresh))
                  (loop (cdr t-list) tier thresh)
                  (loop (cdr t-list) best max-thresh)
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%round6 n)
      (/ (round (* n 1000000.0)) 1000000.0)
    ) ;define

    (define (pi-calculate-cost cost-rates usage)
      (let* ((in-tok (%val 'input usage 0))
             (out-tok (%val 'output usage 0))
             (cr-tok (%val 'cacheRead usage 0))
             (cw-tok (%val 'cacheWrite usage 0))
             (tot-in (+ in-tok cr-tok cw-tok))
             (rates (%resolve-cost-tier cost-rates tot-in))
             (r-in (%val 'input rates 0.0))
             (r-out (%val 'output rates 0.0))
             (r-cr (%val 'cacheRead rates 0.0))
             (r-cw (%val 'cacheWrite rates 0.0))
             (c-in (%round6 (/ (* r-in in-tok) 1000000.0)))
             (c-out (%round6 (/ (* r-out out-tok) 1000000.0)))
             (c-cr (%round6 (/ (* r-cr cr-tok) 1000000.0)))
             (c-cw (%round6 (/ (* r-cw cw-tok) 1000000.0)))
             (c-tot (%round6 (+ c-in c-out c-cr c-cw)))
            ) ;
        (list (cons 'input c-in)
          (cons 'output c-out)
          (cons 'cacheRead c-cr)
          (cons 'cacheWrite c-cw)
          (cons 'total c-tot)
        ) ;list
      ) ;let*
    ) ;define

    (define (pi-calculate-cache-savings cost-rates usage)
      (let* ((in-tok (%val 'input usage 0))
             (cr-tok (%val 'cacheRead usage 0))
             (cw-tok (%val 'cacheWrite usage 0))
             (tot-in (+ in-tok cr-tok cw-tok))
             (rates (%resolve-cost-tier cost-rates tot-in))
             (r-in (%val 'input rates 0.0))
             (r-cr (%val 'cacheRead rates 0.0))
             (diff (max 0.0 (- r-in r-cr)))
            ) ;
        (%round6 (/ (* diff cr-tok) 1000000.0))
      ) ;let*
    ) ;define

    (define (pi-summarize-session-cost cost-rates usages)
      (let ((totals
              (fold
                (lambda (u acc)
                  (cons
                    (+ (car acc) (%val 'total (pi-calculate-cost cost-rates u) 0.0))
                    (+ (cdr acc) (pi-calculate-cache-savings cost-rates u))
                  ) ;cons
                ) ;lambda
                '(0.0 . 0.0)
                usages
              ) ;fold
            ) ;totals
           ) ;
        (list (cons 'totalCost (%round6 (car totals)))
          (cons 'totalSavings (%round6 (cdr totals)))
        ) ;list
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
