;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` all-in-one 子标签页的交互覆盖层：
;;   1. new-chain：输入新伪模型（chain）id 并创建空链；
;;   2. slot-picker：从已启用供应商及其已启用模型与思考等级中选择并追加新槽位；
;;   3. thinking-picker：调整已有槽位的思考强度。
(define-library (liii pi code modes config aio-modal)
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii list)
    (liii string)
    (liii pi ai thinking)
    (liii pi code config user-fs)
    (liii pi code modes config state)
    (liii pi code modes config all-in-one)
    (liii pi code modes config aio-writes)
  ) ;import
  (export pi-config-aio-modal-new-chain pi-config-aio-modal-slot-picker
    pi-config-aio-modal-thinking-picker pi-config-aio-modal-step
  ) ;export
  (begin

    (define (%ref alist key default)
      (let ((pair (and (pair? alist) (assoc key alist))))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%clamp idx count)
      (cond ((<= count 0) 0)
            ((< idx 0) 0)
            ((>= idx count) (- count 1))
            (else idx)
      ) ;cond
    ) ;define

    (define (%key-str key)
      (cond ((char? key) (string key))
            ((symbol? key) (symbol->string key))
            (else key)
      ) ;cond
    ) ;define

    (define (%step-result modal writes message select-chain)
      (list (cons 'modal modal)
        (cons 'writes writes)
        (cons 'message message)
        (cons 'select-chain select-chain)
      ) ;list
    ) ;define

    (define (%apply-plan-step plan)
      (%step-result #f
        (pi-user-dict-ref plan "writes" '())
        (pi-user-dict-ref plan "message" "")
        #f
      ) ;%step-result
    ) ;define

    ;; ---------- 构造 ----------

    (define (%make-new-chain input err)
      (list (cons 'mode 'new-chain) (cons 'input input) (cons 'error err))
    ) ;define

    (define (%make-picker mode chain-id targets p-idx m-idx t-idx err)
      (list (cons 'mode mode)
        (cons 'chain-id chain-id)
        (cons 'targets targets)
        (cons 'p-idx p-idx)
        (cons 'm-idx m-idx)
        (cons 't-idx t-idx)
        (cons 'error err)
      ) ;list
    ) ;define

    (define (%make-slot-thinking chain-id node-index provider-id model-id t-idx)
      (list (cons 'mode 'pick-slot-thinking)
        (cons 'chain-id chain-id)
        (cons 'node-index node-index)
        (cons 'provider provider-id)
        (cons 'model model-id)
        (cons 't-idx t-idx)
      ) ;list
    ) ;define

    (define (pi-config-aio-modal-new-chain)
      (%make-new-chain "" #f)
    ) ;define

    (define (pi-config-aio-modal-slot-picker st chain-id)
      (let ((targets (pi-config-aio-enabled-targets st)))
        (%make-picker 'pick-provider
          chain-id
          targets
          0
          0
          0
          (if (null? targets) "no enabled providers/models" #f)
        ) ;%make-picker
      ) ;let
    ) ;define

    (define (pi-config-aio-modal-thinking-picker chain-id node-index provider-id
              model-id cur-level
            ) ;pi-config-aio-modal-thinking-picker
      (let ((idx
              (or
                (list-index (lambda (x) (string=? x (or cur-level "off"))) pi-thinking-levels)
                0
              ) ;or
            ) ;idx
           ) ;
        (%make-slot-thinking chain-id node-index provider-id model-id idx)
      ) ;let
    ) ;define

    ;; ---------- 键盘步进 ----------

    (define (%id-char? s)
      (and (string? s)
        (= (string-length s) 1)
        (let ((ch (string-ref s 0)))
          (or (char-alphabetic? ch)
            (char-numeric? ch)
            (char=? ch #\-)
            (char=? ch #\_)
            (char=? ch #\.)
          ) ;or
        ) ;let
      ) ;and
    ) ;define

    (define (%step-new-chain st modal key)
      (let ((input (%ref modal 'input "")) (c (%key-str key)))
        (cond ((or (eq? key 'escape) (eq? key 'ctrl-c)) (%step-result #f '() "" #f))
              ((eq? key 'backspace)
               (let ((next
                       (if (> (string-length input) 0)
                         (substring input 0 (- (string-length input) 1))
                         ""
                       ) ;if
                     ) ;next
                    ) ;
                 (%step-result (%make-new-chain next #f) '() "" #f)
               ) ;let
              ) ;
              ((eq? key 'enter)
               (let ((plan (pi-config-aio-add-chain st input)))
                 (if (pi-user-dict-ref plan "ok" #f)
                   (%step-result #f
                     (pi-user-dict-ref plan "writes" '())
                     (pi-user-dict-ref plan "message" "")
                     (string-trim-both input)
                   ) ;%step-result
                   (%step-result (%make-new-chain input (pi-user-dict-ref plan "message" "invalid id"))
                     '()
                     ""
                     #f
                   ) ;%step-result
                 ) ;if
               ) ;let
              ) ;
              ((%id-char? c)
               (%step-result (%make-new-chain (string-append input c) #f) '() "" #f)
              ) ;
              (else (%step-result modal '() "" #f))
        ) ;cond
      ) ;let
    ) ;define

    (define (%step-pick-provider modal key)
      (let* ((targets (%ref modal 'targets '()))
             (count (length targets))
             (p-idx (%clamp (%ref modal 'p-idx 0) count))
             (chain-id (%ref modal 'chain-id ""))
             (c (%key-str key))
            ) ;
        (cond ((or (eq? key 'escape) (eq? key 'ctrl-c) (equal? c "q"))
               (%step-result #f '() "" #f)
              ) ;
              ((null? targets) (%step-result modal '() "" #f))
              ((or (eq? key 'up) (equal? c "k"))
               (%step-result (%make-picker 'pick-provider chain-id targets (%clamp (- p-idx 1) count) 0 0 #f)
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'down) (equal? c "j"))
               (%step-result (%make-picker 'pick-provider chain-id targets (%clamp (+ p-idx 1) count) 0 0 #f)
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'enter) (eq? key 'right) (equal? c "l"))
               (%step-result (%make-picker 'pick-model chain-id targets p-idx 0 0 #f)
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              (else (%step-result modal '() "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%step-pick-model st modal key)
      (let* ((targets (%ref modal 'targets '()))
             (p-idx (%clamp (%ref modal 'p-idx 0) (length targets)))
             (p-entry (list-ref targets p-idx))
             (p-id (car p-entry))
             (models (cdr p-entry))
             (m-count (length models))
             (m-idx (%clamp (%ref modal 'm-idx 0) m-count))
             (chain-id (%ref modal 'chain-id ""))
             (c (%key-str key))
            ) ;
        (cond ((or (eq? key 'ctrl-c) (equal? c "q")) (%step-result #f '() "" #f))
              ((or (eq? key 'escape) (eq? key 'left) (equal? c "h"))
               (%step-result (%make-picker 'pick-provider chain-id targets p-idx 0 0 #f)
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'up) (equal? c "k"))
               (%step-result (%make-picker 'pick-model
                               chain-id
                               targets
                               p-idx
                               (%clamp (- m-idx 1) m-count)
                               0
                               #f
                             ) ;%make-picker
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'down) (equal? c "j"))
               (%step-result (%make-picker 'pick-model
                               chain-id
                               targets
                               p-idx
                               (%clamp (+ m-idx 1) m-count)
                               0
                               #f
                             ) ;%make-picker
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((eq? key 'enter)
               (let* ((m (list-ref models m-idx)) (m-id (pi-config-model-id m)))
                 (if (pi-config-model-reasoning? m)
                   (let ((med
                           (or (list-index (lambda (x) (string=? x "medium")) pi-thinking-levels) 0)
                         ) ;med
                        ) ;
                     (%step-result (%make-picker 'pick-thinking chain-id targets p-idx m-idx med #f)
                       '()
                       ""
                       #f
                     ) ;%step-result
                   ) ;let
                   (%apply-plan-step (pi-config-aio-add-slot st chain-id p-id m-id "off"))
                 ) ;if
               ) ;let*
              ) ;
              (else (%step-result modal '() "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%step-pick-thinking st modal key)
      (let* ((t-count (length pi-thinking-levels))
             (t-idx (%clamp (%ref modal 't-idx 0) t-count))
             (chain-id (%ref modal 'chain-id ""))
             (targets (%ref modal 'targets '()))
             (p-idx (%clamp (%ref modal 'p-idx 0) (length targets)))
             (p-entry (list-ref targets p-idx))
             (m-idx (%clamp (%ref modal 'm-idx 0) (length (cdr p-entry))))
             (m-id (pi-config-model-id (list-ref (cdr p-entry) m-idx)))
             (c (%key-str key))
            ) ;
        (cond ((or (eq? key 'ctrl-c) (equal? c "q")) (%step-result #f '() "" #f))
              ((or (eq? key 'escape) (eq? key 'left) (equal? c "h"))
               (%step-result (%make-picker 'pick-model chain-id targets p-idx m-idx 0 #f)
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'up) (equal? c "k"))
               (%step-result (%make-picker 'pick-thinking
                               chain-id
                               targets
                               p-idx
                               m-idx
                               (%clamp (- t-idx 1) t-count)
                               #f
                             ) ;%make-picker
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'down) (equal? c "j"))
               (%step-result (%make-picker 'pick-thinking
                               chain-id
                               targets
                               p-idx
                               m-idx
                               (%clamp (+ t-idx 1) t-count)
                               #f
                             ) ;%make-picker
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((eq? key 'enter)
               (%apply-plan-step (pi-config-aio-add-slot st
                                   chain-id
                                   (car p-entry)
                                   m-id
                                   (list-ref pi-thinking-levels t-idx)
                                 ) ;pi-config-aio-add-slot
               ) ;%apply-plan-step
              ) ;
              (else (%step-result modal '() "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%step-pick-slot-thinking st modal key)
      (let* ((t-count (length pi-thinking-levels))
             (t-idx (%clamp (%ref modal 't-idx 0) t-count))
             (chain-id (%ref modal 'chain-id ""))
             (node-index (%ref modal 'node-index 0))
             (p-id (%ref modal 'provider ""))
             (m-id (%ref modal 'model ""))
             (c (%key-str key))
            ) ;
        (cond ((or (eq? key 'escape) (eq? key 'ctrl-c) (equal? c "q"))
               (%step-result #f '() "" #f)
              ) ;
              ((or (eq? key 'up) (equal? c "k"))
               (%step-result (%make-slot-thinking chain-id node-index p-id m-id (%clamp (- t-idx 1) t-count))
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((or (eq? key 'down) (equal? c "j"))
               (%step-result (%make-slot-thinking chain-id node-index p-id m-id (%clamp (+ t-idx 1) t-count))
                 '()
                 ""
                 #f
               ) ;%step-result
              ) ;
              ((eq? key 'enter)
               (%apply-plan-step (pi-config-aio-set-slot-thinking st
                                   chain-id
                                   node-index
                                   (list-ref pi-thinking-levels t-idx)
                                 ) ;pi-config-aio-set-slot-thinking
               ) ;%apply-plan-step
              ) ;
              (else (%step-result modal '() "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-config-aio-modal-step st modal key)
      (let ((mode (%ref modal 'mode 'new-chain)))
        (cond ((eq? mode 'new-chain) (%step-new-chain st modal key))
              ((eq? mode 'pick-provider) (%step-pick-provider modal key))
              ((eq? mode 'pick-model) (%step-pick-model st modal key))
              ((eq? mode 'pick-thinking) (%step-pick-thinking st modal key))
              ((eq? mode 'pick-slot-thinking) (%step-pick-slot-thinking st modal key))
              (else (%step-result #f '() "" #f))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
