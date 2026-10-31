(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-error-result
;; provider 可复用的默认额度耗尽判定实现。
;;
;; 语法
;; ----
;; (pi-quota-error-result status-code body-str table)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码，本实现只认 403 与 429。
;;
;; body-str : string?
;; 响应体原文。
;;
;; table : list?
;; 该 provider 的码表 ((标识 . 额度类型) …)。
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f
;; 命中时返回判定结果 record，否则 #f。
;;
;; 注意
;; ----
;; 只是默认实现：provider 若需要别的状态码、别的字段结构或要解析重置时间，
;; 可在自家模块里自行实现同名签名的处理函数，不必调用它。

(define table '(("1308" . "five-hour") ("1310" . "weekly")))

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
) ;define

;; 1. 命中：429
(let ((limit (pi-quota-error-result 429 weekly-body table)))
  (check (pi-quota-limit-kind limit) => "weekly")
  (check (pi-quota-limit-code limit) => "1310")
  (check (pi-quota-limit-message limit) => "每周额度已用尽")
) ;let

;; 2. 命中：403（部分入口用 403 表示额度耗尽）
(check-true (pi-quota-limit? (pi-quota-error-result 403 weekly-body table)))

;; 3. 状态码不在 {403, 429}：不判定
(check-false (pi-quota-error-result 400 weekly-body table))
(check-false (pi-quota-error-result 500 weekly-body table))
(check-false (pi-quota-error-result 0 weekly-body table))

;; 4. 标识不在码表：不判定
(check-false (pi-quota-error-result 429
               "{\"error\":{\"code\":\"1302\",\"message\":\"x\"}}"
               table
             ) ;pi-quota-error-result
) ;check-false

;; 5. 空响应体 / 非 JSON：不判定
(check-false (pi-quota-error-result 429 "" table))
(check-false (pi-quota-error-result 429 "quota exceeded" table))

(check-report)
