;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话条目到 LLM 上下文消息的单一映射（投影基础）：
;; 抽取自压缩层的 %as-pi-message，固化为纯函数，供投影层独立使用
(define-library (liii pi code core session-manager messages)
  (import (scheme base)
    (liii pi ai message)
    (liii pi code core session-manager normalize)
  ) ;import
  (export pi-entry-to-context-messages)
  (begin

    ;; 会话条目的字段访问：兼容字符串键与符号键，缺失时返回 #f
    (define (%entry-ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    ;; JSON true 解析为符号 true，布尔字段需同时兼容符号与 #t
    (define (%json-true? v)
      (or (eq? #t v) (eq? v 'true))
    ) ;define

    ;; 旧会话兜底：content 缺失或为 JSON null 时按 role 降级为空内容
    ;; （对齐上游 lax content 处理：user 降级为 ""，其余降级为 '()）
    (define (%lax-content role content)
      (cond
       ((and (equal? role "user")
          (or (not content) (eq? content 'null) (null? content))
        ) ;and
        ""
       ) ;
       ((or (not content) (eq? content 'null) (null? content)) '())
       (else content)
      ) ;cond
    ) ;define

    ;; 将单条已解析的会话条目映射为零条或一条 LLM 上下文消息（纯函数，无 I/O）：
    ;; - message 条目按 role 分派：user / assistant / toolResult 转为对应记录；
    ;;   bashExecution 转为 user 文本（命令 + 围栏输出 + 非零退出码行），
    ;;   excludeFromContext（!! 前缀）返回空列表，不进入 LLM 上下文
    ;; - compaction / session 及未知 type 均返回空列表
    ;; - content 缺失、为 null 或为空块列表时降级为空内容，不抛异常
    (define (pi-entry-to-context-messages entry)
      (if (not (equal? (%entry-ref entry "type") "message"))
        '()
        (let* ((msg (pi-normalize-session-msg (%entry-ref entry "message")))
               (role (%entry-ref msg "role"))
              ) ;
          (cond
           ((equal? role "user")
            (list (pi-user-message (%lax-content role (%entry-ref msg "content"))))
           ) ;
           ((equal? role "assistant")
            (list (pi-assistant-message (%lax-content role (%entry-ref msg "content"))
                    (or (%entry-ref msg "stopReason") "stop")
                    (%entry-ref msg "errorMessage")
                    (%entry-ref msg "usage")
                  ) ;pi-assistant-message
            ) ;list
           ) ;
           ((equal? role "toolResult")
            (list (pi-tool-result-message (or (%entry-ref msg "toolCallId") "")
                    (or (%entry-ref msg "toolName") "")
                    (%lax-content role (%entry-ref msg "content"))
                    (%json-true? (%entry-ref msg "isError"))
                  ) ;pi-tool-result-message
            ) ;list
           ) ;
           ((equal? role "bashExecution")
            (if (%json-true? (%entry-ref msg "excludeFromContext"))
              '()
              (list (pi-user-message (pi-bash-execution->user-text (%entry-ref msg "command")
                                       (%entry-ref msg "output")
                                       (%entry-ref msg "exitCode")
                                     ) ;pi-bash-execution->user-text
                    ) ;pi-user-message
              ) ;list
            ) ;if
           ) ;
           (else '())
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
