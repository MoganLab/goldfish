(library (extensible-match code-generator)
  (export decision-tree->syntax)
  (import (rnrs (6))
          (only (srfi :1 lists)
                delete-duplicates
                lset-union
                lset-difference)
          (extensible-match ast)
          (extensible-match decision-tree)
          (extensible-match seq-pattern)
          (extensible-match util))

  ;; The code generator transforms the decision trees (graphs) created
  ;; by the (extenstible-match decision-tree) module into the syntax
  ;; objects for the Scheme code to execute those decision trees.
  ;;
  ;; It does this by partitioning the nodes of the graph into two
  ;; (well, three) sets according to the number of their inward edges.
  ;; Nodes with only one inward edge are compiled directly to the
  ;; Scheme code you’d naïvely expect them to correspond to. Nodes
  ;; with more than one inward edge are compiled to a lambda within a
  ;; letrec. Finally, the root node, which has no inward edges, also
  ;; compiles to the naïve code and is the entry point for the
  ;; compiled code.

  (define (decision-tree->syntax dt)
    (let* ((inedge-counts (dt-count-inedges dt))
           (converging-nodes (dt-converging-nodes inedge-counts)))
      (let-values (((nodes jumps)
                    (hashtable-entries converging-nodes)))
        (let loop ((idx 0)
                   (compilations '()))
          (if (>= idx (vector-length nodes))
              #`(letrec #,compilations
                  #,(decision-tree->syntax* dt converging-nodes))
              (let* ((this-node (vector-ref nodes idx))
                     (this-jump (vector-ref jumps idx))
                     (this-node-id (car this-jump))
                     (this-node-args (cdr this-jump)))
                (loop (+ idx 1)
                      (cons #`(#,this-node-id
                               (lambda #,this-node-args
                                 #,(decision-tree->syntax* this-node
                                                           converging-nodes)))
                            compilations))))))))

  (define (decision-tree->syntax* dt converging-nodes)
    (let recur ((node dt))
      (cond ((and (not (eq? node dt))
                  (hashtable-ref converging-nodes node #f)) => values)
            ((action? node)
             (cons (action-procedure node) (action-args node)))
            ((dt-test? node)
             #`(if (#,(dt-test-proc node) #,(dt-test-var node))
                   #,(recur (dt-node-success-branch node))
                   #,(recur (dt-node-failure-branch node))))
            ((dt-apply? node)
             (if (= (length (dt-apply-vars node)) 1)
                 #`(let ((#,(car (dt-apply-vars node))
                          (#,(dt-apply-proc node)
                           #,(dt-apply-var node))))
                     #,(recur (dt-node-success-branch node)))
                 #`(let-values ((#,(dt-apply-vars node)
                                 (#,(dt-apply-proc node)
                                  #,(dt-apply-var node))))
                     #,(recur (dt-node-success-branch node)))))
            ((dt-equal? node)
             #`(if (equal? #,(dt-equal-var node)
                           '#,(datum->syntax #'_ (dt-equal-val node)))
                   #,(recur (dt-node-success-branch node))
                   #,(recur (dt-node-failure-branch node))))
            ((dt-seq? node)
             (case (seq-pattern-type (dt-seq-pattern node))
               ((ordered partial)
                (seq-pattern-expand/nfa
                 (dt-seq-pattern node)
                 seq-subpat->syntax
                 (recur (dt-node-success-branch node))
                 (recur (dt-node-failure-branch node))))
               ((unordered)
                (seq/unordered-pattern-expand
                 (dt-seq-pattern node)
                 seq-subpat->syntax
                 (recur (dt-node-success-branch node))
                 (recur (dt-node-failure-branch node))))))
            (else (assertion-violation 'decision-tree->syntax
                                       "not a decision-tree node"
                                       dt)))))

  (define (seq-subpat->syntax pat success-branch failure-branch)
    (decision-tree->syntax
     (remove-decision-tree-renames
      (pattern->dt-node pat success-branch failure-branch))))

  (define (dt-count-inedges dt)
    (let ((inedge-counts (make-eq-hashtable))
          (visited-nodes (make-eq-hashtable)))
      (define (count-inedge! node)
        (hashtable-update! inedge-counts node (lambda (n) (+ n 1)) 0))
      (let recur ((node dt))
        (unless (or (action? node)
                    (hashtable-contains? visited-nodes node))
          (hashtable-set! visited-nodes node #t)
          (count-inedge! (dt-node-success-branch node))
          (recur (dt-node-success-branch node))
          (unless (dt-apply? node)
            (count-inedge! (dt-node-failure-branch node))
            (recur (dt-node-failure-branch node)))))
      inedge-counts))

  (define (dt-converging-nodes inedge-counts)
    (let-values (((converging-nodes) (make-eq-hashtable))
                 ((nodes inedges) (hashtable-entries inedge-counts)))
      (let loop ((idx 0))
        (cond ((>= idx (vector-length nodes)) converging-nodes)
              ((action? (vector-ref nodes idx)) (loop (+ idx 1)))
              ((> (vector-ref inedges idx) 1)
               (hashtable-set! converging-nodes
                               (vector-ref nodes idx)
                               (cons (generate-identifier)
                                     (dt-needed-bindings (vector-ref nodes idx))))
               (loop (+ idx 1)))
              (else (loop (+ idx 1)))))))

  (define (dt-needed-bindings dt)
    (delete-duplicates (dt-needed-bindings* dt) bound-identifier=?))
  (define (dt-needed-bindings* dt)
    (assert (and (or (dt-node? dt)
                     (action? dt))
                 (not (dt-rename? dt))))
    (cond ((dt-test? dt)
           (cons (dt-test-var dt)
                 (lset-union bound-identifier=?
                             (dt-needed-bindings* (dt-node-success-branch dt))
                             (dt-needed-bindings* (dt-node-failure-branch dt)))))
          ((dt-apply? dt)
           (cons (dt-apply-var dt)
                 (lset-difference bound-identifier=?
                                  (dt-needed-bindings*
                                   (dt-node-success-branch dt))
                                  (dt-apply-vars dt))))
          ((dt-equal? dt)
           (cons (dt-equal-var dt)
                 (lset-union bound-identifier=?
                             (dt-needed-bindings* (dt-node-success-branch dt))
                             (dt-needed-bindings* (dt-node-failure-branch dt)))))
          ((dt-seq? dt)
           (cons (dt-seq-var dt)
                 (lset-difference bound-identifier=?
                  (lset-union bound-identifier=?
                              (dt-needed-bindings* (dt-node-success-branch dt))
                              (dt-needed-bindings* (dt-node-failure-branch dt)))
                  (pattern-vars (dt-seq-pattern dt)))))
          ((action? dt) (action-args dt)))))
