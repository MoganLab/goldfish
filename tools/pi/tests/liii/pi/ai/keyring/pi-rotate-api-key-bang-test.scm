;; pi-rotate-api-key!
;; 触发 API 密钥轮换钩子以获取下一个可用密钥。
;;
;; 语法
;; ----
;; (pi-rotate-api-key! config current-key status)
;; (pi-rotate-api-key! config current-key status opt-port)
;;
;; 参数
;; ----
;; config : alist?
;; 请求配置关联表。
;;
;; current-key : string?
;; 当前失效或遭遇限流的 API 密钥。
;;
;; status : integer?
;; 触发换 key 的 HTTP 状态码（如 429 或 403）。
;;
;; opt-port : port?
;; 可选，日志输出端口。
;;
;; 返回值
;; ----
;; string?、'exhausted 或 #f
;; 成功时返回下一个 API 密钥字符串，池耗尽返回 'exhausted，未启用钩子返回 #f。

(import (liii check) (liii pi ai keyring))

(check-set-mode! 'report-failed)

(check (pi-rotate-api-key! (list (cons 'api-key "k")) "k" 429) => #f)

;; 429/403 委托 rotate-key 钩子换 key
(let ((rotated '()))
  (let ((cfg
          (list (cons 'api-key "k1")
            (cons 'rotate-key
              (lambda (config current-key status)
                (set! rotated (list current-key status))
                "k2"
              ) ;lambda
            ) ;cons
          ) ;list
        ) ;cfg
       ) ;
    (check (pi-rotate-api-key! cfg "k1" 429) => "k2")
    (check rotated => '("k1" 429))
  ) ;let
) ;let

(check-report)
