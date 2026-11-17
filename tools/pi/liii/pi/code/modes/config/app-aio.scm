;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的 all-in-one 子标签页交互逻辑：
;;   链条/节点导航、将选中链条设为默认模型。
(define-library (liii pi code modes config app-aio)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii pi code config user-fs)
    (liii pi code modes config state)
    (liii pi code modes config layout)
    (liii pi code modes config files)
    (liii pi code modes config json-write)
    (liii pi code modes config all-in-one)
    (liii pi code modes config aio-writes)
    (liii pi code modes config aio-modal)
  ) ;import
  (export pi-config-aio-key-handler pi-config-aio-enter pi-config-aio-action
    pi-config-aio-modal-handle pi-config-aio-exec-action
  ) ;export
  (begin

    (define %columns 3)

    (define (%ref alist key)
      (let ((pair (and (pair? alist) (assoc key alist))))
        (if pair (cdr pair) 0)
      ) ;let
    ) ;define

    (define (%clamp index count)
      (cond ((<= count 0) 0)
            ((< index 0) 0)
            ((>= index count) (- count 1))
            (else index)
      ) ;cond
    ) ;define

    (define (%set-aio-cursor cursor tab chain-index node-index)
      (list (cons 'provider (%ref cursor 'provider))
        (cons 'model (%ref cursor 'model))
        (cons 'tab tab)
        (cons 'chain chain-index)
        (cons 'node node-index)
      ) ;list
    ) ;define

    (define (%aio-node-count aio chain-index)
      (let ((chains (pi-config-aio-chains aio)))
        (if (pair? chains)
          (let ((c (list-ref chains (%clamp chain-index (length chains)))))
            (length (pi-config-aio-chain-nodes c))
          ) ;let
          0
        ) ;if
      ) ;let
    ) ;define

    (define (pi-config-aio-key-handler st cursor active key)
      (let* ((aio (pi-config-aio-state st))
             (chains (pi-config-aio-chains aio))
             (c-real-count (length chains))
             (c-count (+ c-real-count 1))
             (c-index (%clamp (%ref cursor 'chain) c-count))
             (on-new-chain? (= c-index c-real-count))
             (n-count (if on-new-chain? 1 (+ (%aio-node-count aio c-index) 1)))
             (n-index (%clamp (%ref cursor 'node) n-count))
             (c (cond ((char? key) (string key))
                      ((symbol? key) (symbol->string key))
                      (else key)
                ) ;cond
             ) ;c
            ) ;
        (cond
         ((or (eq? key 'up) (equal? c "k"))
          (list
            (if (= active 0)
              (%set-aio-cursor cursor 1 (%clamp (- c-index 1) c-count) 0)
              (%set-aio-cursor cursor 1 c-index (%clamp (- n-index 1) n-count))
            ) ;if
            active
            #f
          ) ;list
         ) ;
         ((or (eq? key 'down) (equal? c "j"))
          (list
            (if (= active 0)
              (%set-aio-cursor cursor 1 (%clamp (+ c-index 1) c-count) 0)
              (%set-aio-cursor cursor 1 c-index (%clamp (+ n-index 1) n-count))
            ) ;if
            active
            #f
          ) ;list
         ) ;
         ((or (eq? key 'right) (equal? c "l"))
          (if (and (= active 0) on-new-chain?)
            (list cursor active #f)
            (list (%set-aio-cursor cursor 1 c-index (%clamp n-index n-count))
              (min (+ active 1) (- %columns 1))
              #f
            ) ;list
          ) ;if
         ) ;
         ((or (eq? key 'left) (equal? c "h")) (list cursor (max (- active 1) 0) #f))
         ((eq? key 'tab) (list cursor (modulo (+ active 1) %columns) #f))
         ((equal? c "g")
          (list (if (= active 0)
                  (%set-aio-cursor cursor 1 0 0)
                  (%set-aio-cursor cursor 1 c-index 0)
                ) ;if
            active
            #f
          ) ;list
         ) ;
         ((equal? c "G")
          (list (if (= active 0)
                  (%set-aio-cursor cursor 1 (- c-count 1) 0)
                  (%set-aio-cursor cursor 1 c-index (- n-count 1))
                ) ;if
            active
            #f
          ) ;list
         ) ;
         (else (list cursor active #f))
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-config-aio-action st cursor active key)
      (let* ((aio (pi-config-aio-state st))
             (chains (pi-config-aio-chains aio))
             (c-real-count (length chains))
             (c-index (%clamp (%ref cursor 'chain) (+ c-real-count 1)))
             (on-new-chain? (= c-index c-real-count))
             (chain (and (not on-new-chain?) (pair? chains) (list-ref chains c-index)))
             (chain-id
               (and chain (cdr (or (assoc "id" chain) '(#f . #f))))
             ) ;chain-id
             (nodes (if chain (pi-config-aio-chain-nodes chain) '()))
             (n-real-count (length nodes))
             (n-index (%clamp (%ref cursor 'node) (+ n-real-count 1)))
             (on-add-slot? (= n-index n-real-count))
             (node (and (not on-add-slot?) (pair? nodes) (list-ref nodes n-index)))
             (c (cond ((char? key) (string key))
                      ((symbol? key) (symbol->string key))
                      (else key)
                ) ;cond
             ) ;c
             (add-key? (or (equal? c "a") (equal? c "n")))
             (del-key? (or (equal? c "d") (equal? c "x") (eq? key 'backspace) (eq? key 'delete))
             ) ;del-key?
            ) ;
        (cond
         ((and (= active 0) (or add-key? (and (eq? key 'enter) on-new-chain?)))
          'new-chain
         ) ;
         ((and (= active 0) del-key? chain-id) (cons 'delete-chain chain-id))
         ((and (>= active 1) chain-id (or add-key? (and (eq? key 'enter) on-add-slot?)))
          (cons 'add-slot chain-id)
         ) ;
         ((and (>= active 1) chain-id node del-key?)
          (list 'delete-slot chain-id n-index)
         ) ;
         ((and (>= active 1) chain-id node (or (equal? c " ") (eq? key 'space)))
          (list 'promote-slot chain-id n-index)
         ) ;
         ((and (>= active 1) chain-id node (or (equal? c "t") (eq? key 'enter)))
          (list 'edit-thinking
            chain-id
            n-index
            (cdr (or (assoc "provider" node) '(#f . "")))
            (cdr (or (assoc "model" node) '(#f . "")))
            (cdr (or (assoc "thinking" node) '(#f . "off")))
          ) ;list
         ) ;
         ((and (eq? key 'enter) (= active 0) chain-id) 'set-default)
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%aio-chain-cursor st chain-id)
      (let* ((providers (pi-config-state-providers st))
             (p-idx
               (list-index (lambda (p) (string=? (pi-config-provider-id p) "all-in-one"))
                 providers
               ) ;list-index
             ) ;p-idx
             (models (if p-idx (pi-config-provider-models (list-ref providers p-idx)) '()))
             (m-idx
               (list-index (lambda (m) (string=? (pi-config-model-id m) chain-id)) models)
             ) ;m-idx
            ) ;
        (and p-idx m-idx (pi-config-cursor p-idx m-idx))
      ) ;let*
    ) ;define

    (define (pi-config-aio-enter st cursor)
      (let* ((aio (pi-config-aio-state st))
             (chains (pi-config-aio-chains aio))
             (c-index (%clamp (%ref cursor 'chain) (length chains)))
             (chain (and (pair? chains) (list-ref chains c-index)))
             (chain-id
               (and chain (cdr (or (assoc "id" chain) '(#f . #f))))
             ) ;chain-id
             (target-cursor (and chain-id (%aio-chain-cursor st chain-id)))
            ) ;
        (if (not target-cursor)
          "chain not found in all-in-one provider"
          (let* ((plan (pi-config-plan-set-default st target-cursor 1))
                 (msg (pi-user-dict-ref plan "message" ""))
                ) ;
            (if (not (pi-user-dict-ref plan "ok" #f))
              msg
              (let ((res (pi-config-apply-writes st (pi-user-dict-ref plan "writes" '()))))
                (if (pi-user-dict-ref res "ok" #f)
                  msg
                  (pi-user-dict-ref res "message" "save failed")
                ) ;if
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-config-aio-modal-handle st modal key)
      (let* ((res (pi-config-aio-modal-step st modal key))
             (next-modal (cdr (assoc 'modal res)))
             (writes (cdr (assoc 'writes res)))
             (msg (cdr (assoc 'message res)))
            ) ;
        (if (pair? writes)
          (let* ((r (pi-config-apply-writes st writes))
                 (final-msg (if (pi-user-dict-ref r "ok" #f)
                              msg
                              (pi-user-dict-ref r "message" "save failed")
                            ) ;if
                 ) ;final-msg
                ) ;
            (list next-modal final-msg #t)
          ) ;let*
          (list next-modal "" #f)
        ) ;if
      ) ;let*
    ) ;define

    (define (%apply-aio-plan st plan)
      (let ((msg (pi-user-dict-ref plan "message" "")))
        (if (not (pi-user-dict-ref plan "ok" #f))
          (list #f msg #f)
          (let ((r (pi-config-apply-writes st (pi-user-dict-ref plan "writes" '()))))
            (if (pi-user-dict-ref r "ok" #f)
              (list #f msg #t)
              (list #f (pi-user-dict-ref r "message" "save failed") #f)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-config-aio-exec-action st cursor act)
      (cond ((eq? act 'new-chain) (list (pi-config-aio-modal-new-chain) "" #f))
            ((eq? act 'set-default) (list #f (pi-config-aio-enter st cursor) #t))
            ((and (pair? act) (eq? (car act) 'add-slot))
             (list (pi-config-aio-modal-slot-picker st (cdr act)) "" #f)
            ) ;
            ((and (pair? act) (eq? (car act) 'delete-chain))
             (%apply-aio-plan st (pi-config-aio-delete-chain st (cdr act)))
            ) ;
            ((and (pair? act) (eq? (car act) 'delete-slot))
             (%apply-aio-plan st (pi-config-aio-delete-slot st (cadr act) (caddr act)))
            ) ;
            ((and (pair? act) (eq? (car act) 'promote-slot))
             (%apply-aio-plan st (pi-config-aio-promote-slot st (cadr act) (caddr act)))
            ) ;
            ((and (pair? act) (eq? (car act) 'edit-thinking))
             (list (apply pi-config-aio-modal-thinking-picker (cdr act)) "" #f)
            ) ;
            (else (list #f "" #f))
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
