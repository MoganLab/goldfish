;; pi-providers-write-writes
;; 生成写入 provider 白名单的写入计划。
;;
;; 语法
;; ----
;; (pi-providers-write-writes ids)
;;
;; 参数
;; ----
;; ids : list
;; provider id 字符串列表。
;;
;; 返回值
;; ----
;; list
;; 只包含 models/providers.json 一项的写入计划。

(import (liii check) (liii string) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(let ((writes (pi-providers-write-writes '("google" "kimi-coding"))))
  (check (length writes) => 1)
  (check (car (car writes)) => "models/providers.json")
  (check (string-contains (cdr (car writes)) "\"google\"") => #t)
  (check (string-contains (cdr (car writes)) "\"kimi-coding\"") => #t)
) ;let

(check-report)
