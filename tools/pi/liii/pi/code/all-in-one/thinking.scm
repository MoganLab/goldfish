;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 节点的 Thinking Level 解析与能力收敛：
;; 用户显式等级 > 节点 all-in-one.json 的 thinkingLevel > 缺省 off，
;; 随后必须按真实物理模型能力收敛，探活与业务请求共用同一结果。
(define-library (liii pi code all-in-one thinking)
  (import (scheme base)
    (liii string)
    (liii path)
    (liii pi ai thinking)
    (liii pi code config)
    (liii pi code modes output)
    (liii pi code all-in-one alarm)
  ) ;import
  (export resolve-node-thinking-level pi-log-thinking-clamp!)
  (begin

    ;; 计算节点最终生效的 Thinking Level：
    ;; supports-thinking? = #f 收敛为 off；否则按可用集合 clamp（先向高、再向低）
    (define (resolve-node-thinking-level explicit-level node-default-level
              models-store provider model-id
            ) ;resolve-node-thinking-level
      (let ((requested
              (cond
               ((and (string? explicit-level)
                  (not (string-null? (string-trim-both explicit-level)))
                ) ;and
                explicit-level
               ) ;
               ((and (string? node-default-level)
                  (not (string-null? (string-trim-both node-default-level)))
                ) ;and
                node-default-level
               ) ;
               (else "off")
              ) ;cond
            ) ;requested
           ) ;
        (if (not (pair? models-store))
          (pi-clamp-thinking-level requested pi-thinking-levels)
          (if (pi-model-supports-thinking? models-store provider model-id)
            (pi-clamp-thinking-level requested
              (pi-model-available-thinking-levels models-store provider model-id)
            ) ;pi-clamp-thinking-level
            "off"
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; 显式值被真实节点能力改变时写一条 [THINKING_CLAMP] 按天日志，
    ;; 并通过 cfg 的 emit 通知前台一次（TUI 显示为状态卡片、脚本模式转 stderr）；
    ;; 每个实际节点只提示一次，不随倒计时 tick 重复告警。
    (define (pi-log-thinking-clamp! config chain-id provider model-id requested effective)
      (when (not (equal? requested effective))
        (pi-alarm-log! "THINKING_CLAMP"
          provider
          model-id
          requested
          (string-append "chain=" chain-id " requested=" requested " effective="
            effective
          ) ;string-append
          (path->string (pi-cfg-config-dir config))
        ) ;pi-alarm-log!
        (%notify-clamp! config provider model-id requested effective)
      ) ;when
    ) ;define

    ;; 通知前台收敛结果：有 emit 时走事件（TUI 卡片），否则退回 stderr
    (define (%notify-clamp! config provider model-id requested effective)
      (let ((emit (and (pair? config) (assoc 'emit config))))
        (if (and emit (procedure? (cdr emit)))
         ((cdr emit) 'thinking_clamp (list provider model-id requested effective))
         (display-stderr (string-append "[all-in-one] thinking level clamped: "
                           requested " -> " effective " (" provider "/" model-id
                           ")\n"
                         ) ;string-append
         ) ;display-stderr
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
