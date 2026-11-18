;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `<chain-node>` 领域记录：链内的一个物理候选节点（provider/model/thinkingLevel），
;; 记录定义、构造器、访问器与 JSON 双向转换放在同一文件。
(define-library (liii pi code all-in-one chain-node)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii string)
    (liii pi code config user-fs)
  ) ;import
  (export make-chain-node chain-node? chain-node-provider chain-node-model
    chain-node-thinking-level json->chain-node chain-node->json
    %non-empty-string?
  ) ;export
  (begin

    (define*-record-type <chain-node>
      (make-chain-node provider model (thinking-level "off"))
      chain-node?
      (provider chain-node-provider)
      (model chain-node-model)
      (thinking-level chain-node-thinking-level)
    ) ;define*-record-type

    (define (%non-empty-string? x)
      (and (string? x) (not (string-null? (string-trim-both x))))
    ) ;define

    (define (json->chain-node obj . opt-chain-id)
      (if (chain-node? obj)
        obj
        (let ((chain-id (if (pair? opt-chain-id) (car opt-chain-id) "<unknown>")))
          (if (not (json-object? obj))
            (begin
              (pi-user-warn (string-append "all-in-one: chain \"" chain-id "\" 的 node 必须是对象")
              ) ;pi-user-warn
              #f
            ) ;begin
            (let* ((p (json-ref-string obj "provider" #f))
                   (m (json-ref-string obj "model" #f))
                   (tl-raw (json-ref-string obj "thinkingLevel" "off"))
                   (tl (if (%non-empty-string? tl-raw) tl-raw "off"))
                  ) ;
              (cond ((not (%non-empty-string? p))
                     (pi-user-warn (string-append "all-in-one: chain \""
                                     chain-id
                                     "\" 节点缺少必填字段 provider，已跳过"
                                   ) ;string-append
                     ) ;pi-user-warn
                     #f
                    ) ;
                    ((not (%non-empty-string? m))
                     (pi-user-warn (string-append "all-in-one: chain \""
                                     chain-id
                                     "\" 节点缺少必填字段 model，已跳过"
                                   ) ;string-append
                     ) ;pi-user-warn
                     #f
                    ) ;
                    (else (make-chain-node p m tl))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (chain-node->json node)
      (unless (chain-node? node)
        (type-error "chain-node->json: expected chain-node" node)
      ) ;unless
      (list (cons "provider" (chain-node-provider node))
        (cons "model" (chain-node-model node))
        (cons "thinkingLevel" (chain-node-thinking-level node))
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
