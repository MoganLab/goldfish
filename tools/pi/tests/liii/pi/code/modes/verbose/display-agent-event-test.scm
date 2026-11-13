;; display-agent-event
;; 格式化输出人类可读的 Agent 事件（含 emoji 与轮次/工具标记）。
;;
;; 语法
;; ----
;; (display-agent-event ev [line] [turn-box] [port])
;;
;; 参数
;; ----
;; ev : (or pair? string? pi-event?)
;; 事件对象、alist 或 JSON 字符串。
;;
;; line : string
;; 原始事件文本行（可选）。
;;
;; turn-box : pair?
;; 包含 (轮次 消息缓冲 是否已打印过本条消息首行) 的可变列表。
;;
;; port : output-port?
;; 输出端口（可选，默认当前输出端口）。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii json)
  (liii string)
  (liii pi ai event)
  (liii pi code modes verbose)
) ;import

(check-set-mode! 'report-failed)

;; 1. turn_start 正确格式化输出
(let* ((tb (list 0))
       (ev (string->json "{\"type\":\"turn_start\"}"))
       (out (with-output-to-string (lambda () (display-agent-event ev "" tb))))
      ) ;
  (check (car tb) => 1)
  (check (string-contains? out "💡 [Turn 1]") => #t)
) ;let*

;; 2. 第 2 轮起前面补换行
(let* ((tb (list 1))
       (ev (string->json "{\"type\":\"turn_start\"}"))
       (out (with-output-to-string (lambda () (display-agent-event ev "" tb))))
      ) ;
  (check (car tb) => 2)
  (check out => "\n💡 [Turn 2]\n")
) ;let*

;; 3. tool_execution_start: bash / read / edit / write / other
(let* ((tb (list 1))
       (bash-ev (string->json "{\"type\":\"tool_execution_start\",\"toolName\":\"bash\",\"args\":{\"command\":\"ls -la\"}}"))
       (read-ev (string->json "{\"type\":\"tool_execution_start\",\"toolName\":\"read\",\"args\":{\"path\":\"main.scm\"}}"))
       (edit-ev (string->json "{\"type\":\"tool_execution_start\",\"toolName\":\"edit\",\"args\":{\"path\":\"foo.scm\"}}"))
       (other-ev (string->json "{\"type\":\"tool_execution_start\",\"toolName\":\"custom\",\"args\":{}}"))
       (out-bash (with-output-to-string (lambda () (display-agent-event bash-ev "" tb))))
       (out-read (with-output-to-string (lambda () (display-agent-event read-ev "" tb))))
       (out-edit (with-output-to-string (lambda () (display-agent-event edit-ev "" tb))))
       (out-other (with-output-to-string (lambda () (display-agent-event other-ev "" tb))))
      ) ;
  (check (string-contains? out-bash "💻 [bash] ls -la") => #t)
  (check (string-contains? out-read "📄 [read] main.scm") => #t)
  (check (string-contains? out-edit "📄 [edit] foo.scm") => #t)
  (check (string-contains? out-other "🔧 [custom]") => #t)
) ;let*

;; 4. tool_execution_end: 成功与失败
(let* ((tb (list 1))
       (ok-ev (string->json "{\"type\":\"tool_execution_end\",\"toolName\":\"bash\",\"isError\":false}"))
       (err-ev (string->json "{\"type\":\"tool_execution_end\",\"toolName\":\"bash\",\"isError\":true}"))
       (out-ok (with-output-to-string (lambda () (display-agent-event ok-ev "" tb))))
       (out-err (with-output-to-string (lambda () (display-agent-event err-ev "" tb))))
      ) ;
  (check (string-contains? out-ok "└─ [bash] ✅ 完成") => #t)
  (check (string-contains? out-err "└─ [bash] ❌ 失败") => #t)
) ;let*

;; 5. message_update 逐行打印，首行带 💬 前缀，续行不带
(let* ((tb (list 1))
       (start (string->json "{\"type\":\"message_start\",\"message\":{\"role\":\"assistant\"}}"))
       (upd (string->json "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"第一行\\n第二行\"}}"))
       (end (string->json "{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\"}}"))
      ) ;
  (display-agent-event start "" tb)
  (let ((upd-out (with-output-to-string (lambda () (display-agent-event upd "" tb))))
        (end-out (with-output-to-string (lambda () (display-agent-event end "" tb))))
       ) ;
    (check upd-out => "💬 第一行\n")
    (check end-out => "第二行\n")
  ) ;let
) ;let*

;; 6. agent_end 输出完成标记并刷新尾部
(let* ((tb (list 1))
       (upd (string->json "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"tail\"}}"))
       (aend (string->json "{\"type\":\"agent_end\"}"))
      ) ;
  (display-agent-event upd "" tb)
  (let ((out (with-output-to-string (lambda () (display-agent-event aend "" tb)))))
    (check (string-contains? out "💬 tail") => #t)
    (check (string-contains? out "🏁 [pi] Agent 本阶段结束") => #t)
  ) ;let
) ;let*

;; 7. message_end 带 stopReason=error 时输出 ❌ [error] 错误行
(let* ((tb (list 1))
       (ev (string->json "{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[],\"stopReason\":\"error\",\"errorMessage\":\"SAFETY\"}}"))
       (out (with-output-to-string (lambda () (display-agent-event ev "" tb))))
      ) ;
  (check (string-contains? out "❌ [error] SAFETY") => #t)
) ;let*

(check-report)
