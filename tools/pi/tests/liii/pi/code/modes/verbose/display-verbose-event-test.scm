;; display-verbose-event
;; 便捷调用 display-agent-event 格式化 Agent 事件。
;;
;; 语法
;; ----
;; (display-verbose-event ev turn-box [port])
;;
;; 参数
;; ----
;; ev : (or pair? string? pi-event?)
;; 事件对象。
;;
;; turn-box : pair?
;; 包含轮次与缓冲状态的列表。
;;
;; port : output-port?
;; 输出端口（可选）。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii json)
  (liii string)
  (liii pi code modes verbose)
) ;import

(check-set-mode! 'report-failed)

(let* ((tb (list 0))
       (ev (string->json "{\"type\":\"turn_start\"}"))
       (out (with-output-to-string (lambda () (display-verbose-event ev tb))))
      ) ;
  (check (car tb) => 1)
  (check (string-contains? out "💡 [Turn 1]") => #t)
) ;let*

(check-report)
