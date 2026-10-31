(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-limit?
;; 额度耗尽判定结果的 record 谓词（同一个 record 提供 kind / code / message 三个访问器）。
;;
;; 语法
;; ----
;; (pi-quota-limit? obj)
;; (pi-quota-limit-kind limit) (pi-quota-limit-code limit) (pi-quota-limit-message limit)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象，通常来自 provider 的额度识别函数。
;;
;; limit : pi-quota-limit?
;; 判定结果 record。
;;
;; 返回值
;; ----
;; boolean / string / #f
;; pi-quota-limit? 返回是否为判定结果；kind 为额度窗口类型；code 为命中的错误标识；
;; message 为服务商官方消息原文（可能 #f）。
;;
;; 注意
;; ----
;; 构造用 %make-pi-quota-limit，由 provider 处理函数内部调用，测试与业务代码只需读访问器。

(define limit (%make-pi-quota-limit "weekly" "1310" "每周额度已用尽"))

;; 1. 谓词
(check-true (pi-quota-limit? limit))
(check-false (pi-quota-limit? #f))
(check-false (pi-quota-limit? '()))

;; 2. 三个字段的访问器
(check (pi-quota-limit-kind limit) => "weekly")
(check (pi-quota-limit-code limit) => "1310")
(check (pi-quota-limit-message limit) => "每周额度已用尽")

;; 3. 官方消息缺失时为 #f
(check (pi-quota-limit-message (%make-pi-quota-limit "quota" "x" #f)) => #f)

(check-report)
