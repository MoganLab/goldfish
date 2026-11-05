;; pi-settings-default-tools
;; 获取客户端全局偏好配置 record 的 default-tools 字段。
;;
;; 语法
;; ----
;; (pi-settings-default-tools obj)
;;
;; 参数
;; ----
;; obj : any?
;; 客户端全局偏好配置 record 实例。
;;
;; 返回值
;; ----
;; list?
;; default-tools 字段的值。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(define retry-cfg-a (%make-pi-retry-config #t 5 1500 45000 12))

(define retry-cfg-b (%make-pi-retry-config #f 0 500 10000 3))
(let* ((obj-a (%make-pi-settings "google"
                "gemini-2.5-flash"
                "medium"
                "http://127.0.0.1:7890"
                retry-cfg-a
                '("read" "write")
                "/tmp/pi-session"
                #f
                '(("deepseek/deepseek-v4-pro" . "high"))
              ) ;%make-pi-settings
       ) ;obj-a
       (obj-b (%make-pi-settings "anthropic"
                "claude-sonnet-4"
                "high"
                #f
                retry-cfg-b
                '("bash")
                "~/.pi/session"
                #t
                '()
              ) ;%make-pi-settings
       ) ;obj-b
      ) ;
  (check (pi-settings-default-tools obj-a) => '("read" "write"))
  (check (pi-settings-default-tools obj-b) => '("bash"))
) ;let*

(check-report)
