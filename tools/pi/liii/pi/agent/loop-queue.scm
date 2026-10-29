;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; run-agent-loop 的排队消息轮询与注入助手（steering / follow-up）
(define-library (liii pi agent loop-queue)
  (export %get-poll-fn %poll-config-messages %poll-steering
    %inject-steering %maybe-inject-steering! %queue-before-stop
    %natural-stop-continue
  ) ;export
  (import (scheme base)
    (liii pi ai message)
  ) ;import
  (begin

    ;; 从 config 取轮询回调（由上层注入；缺失时返回 #f）。
    ;; 契约（对齐上游）：不得抛错，无消息时返回空表
    (define (%get-poll-fn config key)
      (let ((pair (and (pair? config) (assoc key config))))
        (and pair (procedure? (cdr pair)) (cdr pair))
      ) ;let
    ) ;define

    ;; 轮询一次并防御性兜底：回调缺失/抛错/返回非表都视为无消息
    (define (%poll-config-messages config key)
      (let ((fn (%get-poll-fn config key)))
        (if fn
          (let ((res (catch #t (lambda () (fn)) (lambda args '()))))
            (if (and (list? res) (pair? res)) res '())
          ) ;let
          '()
        ) ;if
      ) ;let
    ) ;define

    (define (%poll-steering config)
      (%poll-config-messages config 'get-steering-messages)
    ) ;define

    ;; 注入排队消息：文本转 user 消息追加到 msgs 末尾，并 emit 上屏
    (define (%inject-steering msgs steering emit)
      (let ((user-msgs (map pi-user-message steering)))
        (when (procedure? emit)
          (for-each (lambda (m) (emit 'message_start m) (emit 'message_end m)) user-msgs)
        ) ;when
        (append msgs user-msgs)
      ) ;let
    ) ;define

    ;; steering 轮询点 1（每轮请求前）：有消息则注入并返回新列表，否则原样返回
    (define (%maybe-inject-steering! msgs config emit)
      (let ((steering (%poll-steering config)))
        (if (null? steering) msgs (%inject-steering msgs steering emit))
      ) ;let
    ) ;define

    ;; 单队列自然停止前轮询：有消息则注入并返回新列表，无则返回 #f
    (define (%queue-before-stop msgs config emit key)
      (let ((messages (%poll-config-messages config key)))
        (if (null? messages) #f (%inject-steering msgs messages emit))
      ) ;let
    ) ;define

    ;; 自然停止前轮询点：steering 优先，其次 follow-up（对齐上游：
    ;; 无工具调用且无 steering 时才检查 follow-up）；
    ;; 任一有消息则注入、上报 turn_start 并返回新列表，两者皆空返回 #f
    (define (%natural-stop-continue msgs config emit)
      (let ((next
             (or (%queue-before-stop msgs config emit 'get-steering-messages)
               (%queue-before-stop msgs config emit 'get-follow-up-messages)
             ) ;or
            ) ;next
           ) ;
        (when next
          (when (procedure? emit)
            (emit 'turn_start)
          ) ;when
        ) ;when
        next
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
