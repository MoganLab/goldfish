;; sse-event-json
;; 将 SSE 事件记录中的负载数据反序列化为 JSON 结构。
;;
;; 语法
;; ----
;; (sse-event-json event)
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待解析的 SSE 事件记录对象。
;;
;; 返回值
;; ----
;; any
;; 解析自事件数据字符串的 JSON 数据结构。
;;
;; 错误处理
;; ----
;; type-error 当 event 不是 <sse-event> 记录时。

(import (scheme base) (liii check) (liii json) (liii sse))

(check-set-mode! 'report-failed)

;; 1. JSON 对象解析
(let* ((ev (make-sse-event "json" "{\"user\":\"alice\",\"score\":99}"))
       (j (sse-event-json ev))
      ) ;
  (check (json-ref j "user") => "alice")
  (check (json-ref j "score") => 99)
) ;let*

;; 2. 非法参数类型报错
(check-catch 'type-error (sse-event-json "not-an-event"))
(check-catch 'type-error (sse-event-json 42))

(check-report)
