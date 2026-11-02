;; pi-providers-json
;; 把 provider id 列表序列化为白名单 JSON 文本。
;;
;; 语法
;; ----
;; (pi-providers-json ids)
;;
;; 参数
;; ----
;; ids : list
;; provider id 字符串列表。
;;
;; 返回值
;; ----
;; string
;; JSON 数组文本，末尾带换行。

(import (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(check (pi-providers-json '("google" "kimi-coding"))
  =>
  "[\"google\", \"kimi-coding\"]\n"
) ;check

(check (pi-providers-json '()) => "[]\n")

(check-report)
