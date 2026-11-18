;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` all-in-one 子标签页的增删改写盘计划（纯函数，不直接写盘）。
(define-library (liii pi code modes config aio-writes)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii path)
    (liii pi code all-in-one types)
    (liii pi code all-in-one defaults)
    (liii pi code all-in-one loader)
    (liii pi code config json-write)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-aio-add-chain pi-config-aio-add-slot
    pi-config-aio-delete-chain pi-config-aio-delete-slot
    pi-config-aio-set-slot-thinking pi-config-aio-promote-slot
  ) ;export
  (begin

    (define (%load-or-default-cfg st)
      (all-in-one-load-or-default-config (pi-config-state-dir st))
    ) ;define

    (define (%plan-result ok? msg writes)
      (list (cons "ok" ok?) (cons "message" msg) (cons "writes" writes))
    ) ;define

    (define (%with-chains cfg chains)
      (make-all-in-one-cfg (all-in-one-cfg-continuous? cfg)
        (all-in-one-cfg-max-wait-ms cfg)
        (all-in-one-cfg-retry-interval-ms cfg)
        (all-in-one-cfg-probe-timeout-ms cfg)
        chains
      ) ;make-all-in-one-cfg
    ) ;define

    (define (%cfg-write cfg)
      (let ((json-text (string-append (pi-config-json-pretty (all-in-one-cfg->json cfg)) "\n")
            ) ;json-text
           ) ;
        (list (cons "all-in-one.json" json-text))
      ) ;let
    ) ;define

    (define (%remove-at items idx)
      (let loop
        ((rest items) (i 0) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((= i idx) (loop (cdr rest) (+ i 1) acc))
              (else (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%replace-at items idx proc)
      (let loop
        ((rest items) (i 0) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((= i idx) (loop (cdr rest) (+ i 1) (cons (proc (car rest)) acc)))
              (else (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%update-chain-nodes chains chain-id proc)
      (map
        (lambda (c)
          (if (string=? (chain-def-id c) chain-id)
            (make-chain-def (chain-def-id c) (chain-def-name c) (proc (chain-def-nodes c)))
            c
          ) ;if
        ) ;lambda
        chains
      ) ;map
    ) ;define

    (define (pi-config-aio-add-chain st raw-id)
      (let ((id (string-trim-both (or raw-id ""))))
        (if (string-null? id)
          (%plan-result #f "chain id cannot be empty" '())
          (let* ((cfg (%load-or-default-cfg st))
                 (chains (all-in-one-cfg-chains cfg))
                 (dup?
                   (any (lambda (c) (string=? (chain-def-id c) id)) chains)
                 ) ;dup?
                ) ;
            (if dup?
              (%plan-result #f (string-append "chain \"" id "\" already exists") '())
              (%plan-result #t
                (string-append "created chain " id)
                (%cfg-write
                  (%with-chains cfg (append chains (list (make-chain-def id id '()))))
                ) ;%cfg-write
              ) ;%plan-result
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (pi-config-aio-add-slot st chain-id provider-id model-id thinking-level)
      (let* ((cfg (%load-or-default-cfg st))
             (chains (all-in-one-cfg-chains cfg))
             (target
               (find (lambda (c) (string=? (chain-def-id c) chain-id)) chains)
             ) ;target
            ) ;
        (if (not target)
          (%plan-result #f (string-append "chain \"" chain-id "\" not found") '())
          (let* ((new-node (make-chain-node provider-id model-id (or thinking-level "off")))
                 (new-chains
                   (%update-chain-nodes chains chain-id (lambda (ns) (append ns (list new-node))))
                 ) ;new-chains
                ) ;
            (%plan-result #t
              (string-append "added " provider-id "/" model-id " to " chain-id)
              (%cfg-write (%with-chains cfg new-chains))
            ) ;%plan-result
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-config-aio-delete-chain st chain-id)
      (let* ((cfg (%load-or-default-cfg st))
             (chains (all-in-one-cfg-chains cfg))
             (exists?
               (any (lambda (c) (string=? (chain-def-id c) chain-id)) chains)
             ) ;exists?
            ) ;
        (cond ((not exists?)
               (%plan-result #f (string-append "chain \"" chain-id "\" not found") '())
              ) ;
              ((<= (length chains) 1) (%plan-result #f "at least one chain must remain" '()))
              (else
                (let ((rest (filter (lambda (c) (not (string=? (chain-def-id c) chain-id))) chains)))
                  (%plan-result #t
                    (string-append "deleted chain " chain-id)
                    (%cfg-write (%with-chains cfg rest))
                  ) ;%plan-result
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-config-aio-delete-slot st chain-id node-index)
      (let* ((cfg (%load-or-default-cfg st))
             (chains (all-in-one-cfg-chains cfg))
             (target
               (find (lambda (c) (string=? (chain-def-id c) chain-id)) chains)
             ) ;target
             (nodes (and target (chain-def-nodes target)))
            ) ;
        (cond ((not target)
               (%plan-result #f (string-append "chain \"" chain-id "\" not found") '())
              ) ;
              ((or (< node-index 0) (>= node-index (length nodes)))
               (%plan-result #f "slot index out of range" '())
              ) ;
              (else
                (let ((new-chains (%update-chain-nodes chains chain-id (lambda (ns) (%remove-at ns node-index)))
                      ) ;new-chains
                     ) ;
                  (%plan-result #t
                    (string-append "deleted slot #"
                      (number->string (+ node-index 1))
                      " from "
                      chain-id
                    ) ;string-append
                    (%cfg-write (%with-chains cfg new-chains))
                  ) ;%plan-result
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-config-aio-set-slot-thinking st chain-id node-index level)
      (let* ((cfg (%load-or-default-cfg st))
             (chains (all-in-one-cfg-chains cfg))
             (target
               (find (lambda (c) (string=? (chain-def-id c) chain-id)) chains)
             ) ;target
             (nodes (and target (chain-def-nodes target)))
            ) ;
        (cond ((not target)
               (%plan-result #f (string-append "chain \"" chain-id "\" not found") '())
              ) ;
              ((or (< node-index 0) (>= node-index (length nodes)))
               (%plan-result #f "slot index out of range" '())
              ) ;
              (else
                (let ((new-chains
                        (%update-chain-nodes chains
                          chain-id
                          (lambda (ns)
                            (%replace-at ns
                              node-index
                              (lambda (n)
                                (make-chain-node (chain-node-provider n) (chain-node-model n) level)
                              ) ;lambda
                            ) ;%replace-at
                          ) ;lambda
                        ) ;%update-chain-nodes
                      ) ;new-chains
                     ) ;
                  (%plan-result #t
                    (string-append "set slot #"
                      (number->string (+ node-index 1))
                      " thinking to "
                      level
                    ) ;string-append
                    (%cfg-write (%with-chains cfg new-chains))
                  ) ;%plan-result
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (%promote-node nodes idx)
      (cons (list-ref nodes idx) (%remove-at nodes idx))
    ) ;define

    (define (pi-config-aio-promote-slot st chain-id node-index)
      (let* ((cfg (%load-or-default-cfg st))
             (chains (all-in-one-cfg-chains cfg))
             (target
               (find (lambda (c) (string=? (chain-def-id c) chain-id)) chains)
             ) ;target
             (nodes (and target (chain-def-nodes target)))
            ) ;
        (cond ((not target)
               (%plan-result #f (string-append "chain \"" chain-id "\" not found") '())
              ) ;
              ((or (< node-index 0) (>= node-index (length nodes)))
               (%plan-result #f "slot index out of range" '())
              ) ;
              ((= node-index 0) (%plan-result #t "slot #1 is already first" '()))
              (else
                (let* ((node (list-ref nodes node-index))
                       (new-chains (%update-chain-nodes chains
                                     chain-id
                                     (lambda (ns) (%promote-node ns node-index))
                                   ) ;%update-chain-nodes
                       ) ;new-chains
                      ) ;
                  (%plan-result #t
                    (string-append "moved "
                      (chain-node-provider node)
                      "/"
                      (chain-node-model node)
                      " to #1 in "
                      chain-id
                    ) ;string-append
                    (%cfg-write (%with-chains cfg new-chains))
                  ) ;%plan-result
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
