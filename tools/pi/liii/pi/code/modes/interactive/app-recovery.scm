;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 全链故障等待与思考等级收敛的前台提示：
;; recovery 事件原地更新同一张状态卡片，收敛提示追加一次性卡片。
;; 两者都只是界面提示，不写入会话 JSONL，也不产生 user/assistant 消息。
(define-library (liii pi code modes interactive app-recovery)
  (export pi-app-handle-status-event! pi-app-reset-recovery-card!)
  (import (scheme base)
    (liii string)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive status-card)
  ) ;import
  (begin

    (define (%invalidate-scroll! app)
      (scroll-view-invalidate! (pi-app-scroll-view app))
    ) ;define

    ;; 无等待卡片时新建一张，否则原地改文案（连续 tick 不堆积卡片）
    (define (pi-app-update-recovery-card! app text)
      (%invalidate-scroll! app)
      (let ((card (pi-app-recovery-card app)))
        (if card
          (status-card-set-message! card text)
          (let ((new-card (make-status-card text)))
            (%pi-app-recovery-card-set! app new-card)
            (container-add! (%pi-app-chat app) new-card)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-app-reset-recovery-card! app)
      (%pi-app-recovery-card-set! app #f)
    ) ;define

    (define (%handle-recovery! app ev)
      (let ((chain-id (cadr ev)) (reason (if (> (length ev) 2) (caddr ev) #f)))
        (case (car ev)
              ((recovery_wait recovery_tick)
               (pi-app-update-recovery-card! app
                 (string-append "All nodes in "
                   chain-id
                   " failed. Retrying in "
                   (number->string (quotient (+ reason 999) 1000))
                   "s... (Esc or type a message to stop waiting)"
                 ) ;string-append
               ) ;pi-app-update-recovery-card!
              ) ;
              ((recovery_retry)
               (pi-app-update-recovery-card! app
                 (string-append "Retrying all-in-one chain " chain-id "...")
               ) ;pi-app-update-recovery-card!
              ) ;
              ((recovery_end)
               (let ((card (pi-app-recovery-card app)))
                 (when card
                   (status-card-set-message! card
                     (if (eq? reason 'success)
                       (string-append "Chain " chain-id " recovered.")
                       (string-append "Chain " chain-id " recovery ended: " (object->string reason))
                     ) ;if
                   ) ;status-card-set-message!
                   (pi-app-reset-recovery-card! app)
                 ) ;when
               ) ;let
              ) ;
        ) ;case
      ) ;let
    ) ;define

    ;; 显式等级被节点能力收敛：一次性提示卡片，说明请求值与实际生效值
    (define (%handle-thinking-clamp! app ev)
      (let ((info (cadr ev)))
        (when (and (pair? info) (>= (length info) 4))
          (%invalidate-scroll! app)
          (container-add! (%pi-app-chat app)
            (make-assistant-card (string-append "Thinking level clamped: "
                                   (list-ref info 2)
                                   " -> "
                                   (list-ref info 3)
                                   " ("
                                   (car info)
                                   "/"
                                   (cadr info)
                                   ")"
                                 ) ;string-append
            ) ;make-assistant-card
          ) ;container-add!
        ) ;when
      ) ;let
    ) ;define

    (define (pi-app-handle-status-event! app ev)
      (case (car ev)
            ((thinking_clamp) (%handle-thinking-clamp! app ev))
            (else (%handle-recovery! app ev))
      ) ;case
    ) ;define

  ) ;begin
) ;define-library
