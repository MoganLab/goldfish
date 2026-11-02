;; pi-request-provider-stream
;; 根据服务商或配置中声明的协议类型将流式请求路由派发到对应后端。
;;
;; 语法
;; ----
;; (pi-request-provider-stream provider config msgs sys-prompt tools on-ev timeout)
;;
;; 参数
;; ----
;; provider : string?
;; 服务商名称（如 "deepseek"、"anthropic"、"google" 等）。
;;
;; config : alist?
;; 请求配置关联表（可包含 provider-api 等字段）。
;;
;; msgs : list?
;; 消息列表。
;;
;; sys-prompt : string? 或 #f
;; 系统提示词。
;;
;; tools : list?
;; 工具定义列表。
;;
;; on-ev : procedure?
;; 事件回调处理过程。
;;
;; timeout : number?
;; 请求超时时间（秒）。
;;
;; 返回值
;; ----
;; any?
;; 对应后端协议流式请求执行结果。

(import (liii check) (liii pi ai dispatch))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-request-provider-stream))

(check-report)
