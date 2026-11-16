;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message user)
  (import (scheme base) (liii base) (liii string) (liii pi ai time))
  (export pi-user-message pi-user-message? pi-user-message-role
    pi-user-message-content pi-user-message-timestamp
    pi-bash-execution->user-text
  ) ;export
  (begin

    (define*-record-type <pi-user-message>
      (pi-user-message content (timestamp (pi-current-time-millis)))
      pi-user-message?
      (content pi-user-message-content)
      (timestamp pi-user-message-timestamp)
    ) ;define*-record-type

    (define (pi-user-message-role msg)
      "user"
    ) ;define

    ;; 将 bashExecution 消息转为注入 LLM 上下文的 user 消息文本
    ;; （对齐 pi 的 bashExecutionToText）：
    ;;   Ran `cmd` + 围栏输出（空输出为 (no output)）+ 非零退出码行
    (define (pi-bash-execution->user-text command output exit-code)
      (let ((base (string-append "Ran `"
                    command
                    "`\n"
                    (if (string-null? output) "(no output)" (string-append "```\n" output "\n```"))
                  ) ;string-append
            ) ;base
           ) ;
        (if (and (number? exit-code) (not (= exit-code 0)))
          (string-append base "\n\nCommand exited with code " (number->string exit-code))
          base
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
