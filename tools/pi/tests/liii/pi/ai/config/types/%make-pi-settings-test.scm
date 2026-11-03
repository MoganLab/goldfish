;; %make-pi-settings
;; 构造客户端全局偏好配置 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-settings default-provider default-model default-thinking-level http-proxy retry default-tools session-dir hide-thinking-block model-thinking-levels)
;;
;; 参数
;; ----
;; default-provider : string? 或 #f
;; 默认使用的服务商名称
;;
;; default-model : string? 或 #f
;; 默认使用的模型标识符
;;
;; default-thinking-level : string? 或 #f
;; 默认思考级别，如 off、low、medium、high
;;
;; http-proxy : string? 或 #f
;; HTTP 代理地址，未配置时为 #f
;;
;; retry : <pi-retry-config>
;; 重试策略配置
;;
;; default-tools : list?
;; 默认启用的工具名称列表
;;
;; session-dir : string? 或 #f
;; 会话数据保存目录
;;
;; hide-thinking-block : boolean?
;; 是否隐藏思考内容块
;;
;; model-thinking-levels : alist?
;; 按模型标识单独覆盖的思考级别关联表
;;
;; 返回值
;; ----
;; pi-settings?
;; 客户端全局偏好配置 record 实例。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(define retry-cfg-a (%make-pi-retry-config #t 5 1500 45000 12))
(let ((obj (%make-pi-settings "google"
             "gemini-2.5-flash"
             "medium"
             "http://127.0.0.1:7890"
             retry-cfg-a
             '("read" "write")
             "/tmp/pi-session"
             #f
             '(("deepseek/deepseek-v4-pro" . "high"))
           ) ;%make-pi-settings
      ) ;obj
     ) ;
  (check-true (pi-settings? obj))
  (check (pi-settings-default-provider obj) => "google")
  (check (pi-settings-default-model obj) => "gemini-2.5-flash")
  (check (pi-settings-default-thinking-level obj) => "medium")
  (check (pi-settings-http-proxy obj) => "http://127.0.0.1:7890")
  (check-true (eq? (pi-settings-retry obj) retry-cfg-a))
  (check (pi-settings-default-tools obj) => '("read" "write"))
  (check (pi-settings-session-dir obj) => "/tmp/pi-session")
  (check (pi-settings-hide-thinking-block obj) => #f)
  (check (pi-settings-model-thinking-levels obj)
    =>
    '(("deepseek/deepseek-v4-pro" . "high"))
  ) ;check
) ;let

(check-report)
