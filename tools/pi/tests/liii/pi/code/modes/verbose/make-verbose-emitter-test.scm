;; make-verbose-emitter
;; 构造一个符合 pi emit 规范的人类可读 verbose 事件发射器过程。
;;
;; 语法
;; ----
;; (make-verbose-emitter [port])
;;
;; 参数
;; ----
;; port : output-port?
;; 输出端口（可选，默认当前输出端口）。
;;
;; 返回值
;; ----
;; procedure?
;; 事件发射器，接收 (ev-type . args)。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code modes verbose)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证返回值是可调用的过程
(let ((emitter (make-verbose-emitter)))
  (check-true (procedure? emitter))
) ;let

;; 2. 调用 emitter 输出 turn_start 与工具调用
(let* ((emitter (make-verbose-emitter))
       (out (with-output-to-string
              (lambda ()
                (emitter 'turn_start)
                (emitter 'tool_execution_start "id1" "bash" '((command . "git status")))
                (emitter 'tool_execution_end "id1" "bash" '() #f 100)
              ) ;lambda
            ) ;with-output-to-string
       ) ;out
      ) ;
  (check-true (string-contains? out "💡 [Turn 1]"))
  (check-true (string-contains? out "💻 [bash] git status"))
  (check-true (string-contains? out "└─ [bash] ✅ 完成"))
) ;let*

;; 3. 消息文本流式更新与完成
(let* ((emitter (make-verbose-emitter))
       (msg (pi-assistant-message (list (pi-text-block "Hello world"))))
       (out (with-output-to-string
              (lambda ()
                (emitter 'turn_start)
                (emitter 'message_start msg)
                (emitter 'message_update '() '((type . "text_delta") (delta . "Hello stream\n")))
                (emitter 'message_end msg)
                (emitter 'agent_end (list msg) #f 0 0)
              ) ;lambda
            ) ;with-output-to-string
       ) ;out
      ) ;
  (check-true (string-contains? out "💡 [Turn 1]"))
  (check-true (string-contains? out "💬 Hello stream"))
  (check-true (string-contains? out "🏁 [pi] Agent 本阶段结束"))
) ;let*

(check-report)
