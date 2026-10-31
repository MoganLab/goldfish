(import (liii check)
  (liii pi ai providers quota)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; pi-quota-message-trailing-code
;; 从官方错误消息尾部提取形如 "(2067)" 的业务码。
;;
;; 语法
;; ----
;; (pi-quota-message-trailing-code message)
;;
;; 参数
;; ----
;; message : string? 或 #f
;; 服务商官方消息原文。
;;
;; 返回值
;; ----
;; 字符串（纯数字，如 "2067"）或 #f
;; 消息以 "(数字)" 结尾时返回括号内的数字串，否则 #f。
;;
;; 注意
;; ----
;; MiniMax 等服务商把业务码附在消息末尾（"... (2067)"），没有独立的
;; code 字段。数字码是稳定标识，不受文案改写与本地化影响；判定依据
;; 是码本身而不是文案，符合本模块「不用文案判定」的约定。

;; 1. 真实响应：Token Plan 用量上限，尾码 2067
(check
  (pi-quota-message-trailing-code
    "当前已达到 Token Plan 用量上限。为避免调用中断，请升级 Token Plan 套餐，或购买积分补充用量并开启积分自动消耗。 (2067)"
  ) ;pi-quota-message-trailing-code
  => "2067"
) ;check

;; 2. 普通括号数字
(check (pi-quota-message-trailing-code "Rate limit (12)") => "12")

;; 3. 尾部空白不影响
(check (pi-quota-message-trailing-code "(2067) ") => "2067")

;; 4. 码在中间不算
(check-false (pi-quota-message-trailing-code "(2067) extra"))

;; 5. 括号内非纯数字不算
(check-false (pi-quota-message-trailing-code "failed (abc)"))
(check-false (pi-quota-message-trailing-code "failed (20 67)"))

;; 6. 空括号不算
(check-false (pi-quota-message-trailing-code "failed ()"))

;; 7. 无括号
(check-false (pi-quota-message-trailing-code "plain message"))
(check-false (pi-quota-message-trailing-code ""))

;; 8. 非字符串入参
(check-false (pi-quota-message-trailing-code #f))

(check-report)
