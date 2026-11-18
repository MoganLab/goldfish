;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `<chain-def>` 领域记录：一条链式定义（链 ID 即伪模型 ID）及其候选节点列表，
;; 记录定义、构造器、访问器与 JSON 双向转换放在同一文件。
(define-library (liii pi code all-in-one chain-def)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii string)
    (liii pi code all-in-one chain-node)
    (liii pi code config user-fs)
  ) ;import
  (export make-chain-def chain-def? chain-def-id chain-def-name chain-def-nodes
    json->chain-def chain-def->json
  ) ;export
  (begin

    (define*-record-type <chain-def>
      (make-chain-def id name (nodes '()))
      chain-def?
      (id chain-def-id)
      (name chain-def-name)
      (nodes chain-def-nodes)
    ) ;define*-record-type

    (define (json->chain-def obj)
      (if (chain-def? obj)
        obj
        (if (not (json-object? obj))
          (begin
            (pi-user-warn "all-in-one: chain 必须是对象")
            #f
          ) ;begin
          (let* ((id (json-ref-string obj "id" #f))
                 (name (json-ref-string obj "name" #f))
                 (raw-nodes (json-ref obj "nodes"))
                 (nodes-list (pi-user-vector->list raw-nodes))
                ) ;
            (if (not (%non-empty-string? id))
              (begin
                (pi-user-warn "all-in-one: chain 缺少必填字段 id")
                #f
              ) ;begin
              (let* ((eff-name (if (%non-empty-string? name) name id))
                     (valid-nodes
                       (if (not (list? nodes-list))
                         (begin
                           (pi-user-warn (string-append "all-in-one: chain \"" (or id "") "\" 的 nodes 必须是数组")
                           ) ;pi-user-warn
                           '()
                         ) ;begin
                         (filter (lambda (x) x) (map (lambda (n) (json->chain-node n id)) nodes-list))
                       ) ;if
                     ) ;valid-nodes
                    ) ;
                (make-chain-def id eff-name valid-nodes)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (chain-def->json chain)
      (unless (chain-def? chain)
        (type-error "chain-def->json: expected chain-def" chain)
      ) ;unless
      (list (cons "id" (chain-def-id chain))
        (cons "name" (chain-def-name chain))
        (cons "nodes" (list->vector (map chain-node->json (chain-def-nodes chain))))
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
