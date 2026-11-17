;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的 all-in-one 子标签页三列纯文本渲染。
(define-library (liii pi code modes config layout-aio)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii pi code modes config all-in-one)
  ) ;import
  (export pi-config-aio-render-body)
  (begin

    (define (%ref alist key default)
      (let ((pair (and (pair? alist) (assoc key alist))))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%action? item)
      (and (pair? item) (eq? (car item) 'aio-action))
    ) ;define

    (define (pi-config-aio-render-body aio cursor active w1 w2 w3 content-rows
              box-fn list-fn entry-fn detail-fn
            ) ;pi-config-aio-render-body
      (let* ((chains (pi-config-aio-chains aio))
             (chain-items (append chains (list (cons 'aio-action 'new-chain))))
             (c-index (min (max 0 (%ref cursor 'chain 0)) (- (length chain-items) 1)))
             (c-item (list-ref chain-items c-index))
             (chain (and (not (%action? c-item)) c-item))
             (chain-id (if chain (%ref chain "id" "—") "+ new chain"))
             (nodes (if chain (pi-config-aio-chain-nodes chain) '()))
             (node-items
               (if chain (append nodes (list (cons 'aio-action 'add-slot))) '())
             ) ;node-items
             (n-index
               (if (pair? node-items)
                 (min (max 0 (%ref cursor 'node 0)) (- (length node-items) 1))
                 0
               ) ;if
             ) ;n-index
             (n-item (and (pair? node-items) (list-ref node-items n-index)))
             (node (and n-item (not (%action? n-item)) n-item))
             (col1
               (box-fn (string-append "chains · " (number->string (length chains)))
                 (list-fn chain-items
                   content-rows
                   c-index
                   (lambda (item selected?)
                     (if (%action? item)
                       (entry-fn "+ new chain..." #t #f selected?)
                       (entry-fn (%ref item "id" "") #t (%ref item "default" #f) selected?)
                     ) ;if
                   ) ;lambda
                 ) ;list-fn
                 w1
                 (= active 0)
                 content-rows
               ) ;box-fn
             ) ;col1
             (col2
               (box-fn (string-append "nodes · " chain-id)
                 (list-fn node-items
                   content-rows
                   n-index
                   (lambda (item selected?)
                     (if (%action? item)
                       (entry-fn "+ add slot..." #t #f selected?)
                       (entry-fn (string-append (number->string (%ref item "index" 0))
                                   ". "
                                   (%ref item "provider" "")
                                   "/"
                                   (%ref item "model" "")
                                 ) ;string-append
                         (%ref item "enabled" #f)
                         #f
                         selected?
                       ) ;entry-fn
                     ) ;if
                   ) ;lambda
                 ) ;list-fn
                 w2
                 (= active 1)
                 content-rows
               ) ;box-fn
             ) ;col2
             (col3
               (cond ((= w3 0) '())
                     ((not chain)
                      (box-fn "preview · new chain"
                        (detail-fn (list (list "action" "create new pseudo-model chain")
                                     (list "shortcut" "Enter or a")
                                     (list "target" "all-in-one.json")
                                   ) ;list
                        ) ;detail-fn
                        w3
                        (= active 2)
                        content-rows
                      ) ;box-fn
                     ) ;
                     ((and (>= active 1) n-item (%action? n-item))
                      (box-fn (string-append "preview · " chain-id)
                        (detail-fn (list (list "action" "add slot to chain")
                                     (list "shortcut" "Enter or a")
                                     (list "choices" "enabled providers & models")
                                   ) ;list
                        ) ;detail-fn
                        w3
                        (= active 2)
                        content-rows
                      ) ;box-fn
                     ) ;
                     ((and (>= active 1) node)
                      (box-fn (string-append "preview · #"
                                (number->string (%ref node "index" 0))
                                " "
                                (%ref node "model" "")
                              ) ;string-append
                        (detail-fn (pi-config-aio-node-detail-rows aio chain node))
                        w3
                        (= active 2)
                        content-rows
                      ) ;box-fn
                     ) ;
                     (else (box-fn (string-append "preview · " chain-id)
                             (detail-fn (pi-config-aio-chain-detail-rows aio chain))
                             w3
                             (= active 2)
                             content-rows
                           ) ;box-fn
                     ) ;else
               ) ;cond
             ) ;col3
            ) ;
        (if (null? col3)
          (map (lambda (a b) (string-append a b)) col1 col2)
          (map (lambda (a b c) (string-append a b c)) col1 col2 col3)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
