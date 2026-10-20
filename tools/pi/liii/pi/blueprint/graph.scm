;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint graph)
  (import (scheme base) (liii list) (liii hash-table) (liii pi blueprint types))
  (export pi-blueprint-flat-tasks pi-blueprint-find-task
    pi-blueprint-completed-ids pi-blueprint-blocked-ids
    pi-blueprint-blocked-id-list pi-blueprint-ready? pi-blueprint-blocking-tasks
    pi-blueprint-detect-cycles pi-blueprint-topological-sort
  ) ;export
  (begin

    (define (pi-blueprint-flat-tasks phases)
      (apply append (map pi-phase-tasks phases))
    ) ;define

    (define (pi-blueprint-find-task tasks task-id)
      (let ((hit (filter (lambda (t) (string=? (pi-task-id t) task-id)) tasks)))
        (if (pair? hit) (car hit) #f)
      ) ;let
    ) ;define

    (define (pi-blueprint-completed-ids tasks)
      (pi-blueprint-completed-task-ids tasks)
    ) ;define

    (define (pi-blueprint-blocking-tasks tasks task-id)
      (let ((task (pi-blueprint-find-task tasks task-id)))
        (if (not task)
          '()
          (let ((completed (pi-blueprint-completed-ids tasks)))
            (filter (lambda (d) (not (member d completed))) (pi-task-dependencies task))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-blueprint-blocked-ids tasks)
      (filter
        (lambda (t)
          (and (not (pi-task-done? t))
            (pair? (pi-blueprint-blocking-tasks tasks (pi-task-id t)))
          ) ;and
        ) ;lambda
        tasks
      ) ;filter
    ) ;define

    (define (pi-blueprint-blocked-id-list tasks)
      (map pi-task-id (pi-blueprint-blocked-ids tasks))
    ) ;define

    (define (pi-blueprint-ready? tasks task-id)
      (let ((task (pi-blueprint-find-task tasks task-id)))
        (and task
          (not (pi-task-done? task))
          (null? (pi-blueprint-blocking-tasks tasks task-id))
        ) ;and
      ) ;let
    ) ;define

    (define (%hash-of-ids ids default)
      (let ((ht (make-hash-table)))
        (for-each (lambda (id) (hash-table-set! ht id default)) ids)
        ht
      ) ;let
    ) ;define

    ;; DFS 三色标记环检测；返回 [[任务序列] ...]，与 native
    ;; dependency-graph.ts 的 detectCycles 输出形状一致。
    (define (pi-blueprint-detect-cycles tasks)
      (let* ((ids (map pi-task-id tasks))
             (adj
               (map
                 (lambda (t)
                   (cons (pi-task-id t)
                     (filter (lambda (d) (member d ids)) (pi-task-dependencies t))
                   ) ;cons
                 ) ;lambda
                 tasks
               ) ;map
             ) ;adj
             (colors (%hash-of-ids ids 'white))
             (stack '())
             (cycles '())
            ) ;
        (define (%prefix-to target stack)
          (let loop
            ((rest stack) (acc '()))
            (cond ((null? rest) (reverse acc))
                  ((string=? (car rest) target) (reverse (cons target acc)))
                  (else (loop (cdr rest) (cons (car rest) acc)))
            ) ;cond
          ) ;let
        ) ;define
        (define (visit node)
          (hash-table-set! colors node 'gray)
          (set! stack (cons node stack))
          (for-each
            (lambda (nbr)
              (let ((c (hash-table-ref/default colors nbr 'white)))
                (cond
                 ((eq? c 'gray) (set! cycles (append cycles (list (%prefix-to nbr stack)))))
                 ((eq? c 'white) (visit nbr))
                ) ;cond
              ) ;let
            ) ;lambda
            (cdr (assoc node adj))
          ) ;for-each
          (set! stack (cdr stack))
          (hash-table-set! colors node 'black)
        ) ;define
        (for-each
          (lambda (id)
            (when (eq? (hash-table-ref/default colors id 'white) 'white)
              (visit id)
            ) ;when
          ) ;lambda
          ids
        ) ;for-each
        cycles
      ) ;let*
    ) ;define

    ;; Kahn 拓扑排序：返回依赖在前的任务 ID 列表。
    (define (pi-blueprint-topological-sort tasks)
      (let* ((ids (map pi-task-id tasks))
             (in-degree (%hash-of-ids ids 0))
             (children (%hash-of-ids ids '()))
            ) ;
        (for-each
          (lambda (t)
            (for-each
              (lambda (dep)
                (when (member dep ids)
                  (hash-table-set! in-degree
                    (pi-task-id t)
                    (+ 1 (hash-table-ref/default in-degree (pi-task-id t) 0))
                  ) ;hash-table-set!
                  (hash-table-set! children
                    dep
                    (cons (pi-task-id t) (hash-table-ref/default children dep '()))
                  ) ;hash-table-set!
                ) ;when
              ) ;lambda
              (pi-task-dependencies t)
            ) ;for-each
          ) ;lambda
          tasks
        ) ;for-each
        (let loop
          ((queue
             (filter (lambda (id) (= 0 (hash-table-ref/default in-degree id 0))) ids)
           ) ;queue
           (result '())
          ) ;
          (if (null? queue)
            (reverse result)
            (let ((node (car queue)))
              (let ((next-queue (cdr queue)))
                (for-each
                  (lambda (child)
                    (let ((deg (- (hash-table-ref/default in-degree child 0) 1)))
                      (hash-table-set! in-degree child deg)
                      (when (= deg 0)
                        (set! next-queue (append next-queue (list child)))
                      ) ;when
                    ) ;let
                  ) ;lambda
                  (reverse (hash-table-ref/default children node '()))
                ) ;for-each
                (loop next-queue (cons node result))
              ) ;let
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
