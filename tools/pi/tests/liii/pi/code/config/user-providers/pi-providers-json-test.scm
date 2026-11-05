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
;; 多行 JSON 数组文本（逐元素展开、2 空格缩进），末尾带换行。

(import (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(check (pi-providers-json '("google" "kimi-coding"))
  =>
  "[\n  \"google\",\n  \"kimi-coding\"\n]\n"
) ;check

(check (pi-providers-json '()) => "[]\n")

(check-report)
