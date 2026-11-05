;; %keyword-value
;; 从参数列表中提取指定关键字参数的值。
;;
;; 语法
;; ----
;; (%keyword-value key opts default)
;;
;; 参数
;; ----
;; key : any
;; 目标键名，通常为关键字符号（如 :cwd、:env 等）。
;;
;; opts : list
;; 键值对选项参数列表。
;;
;; default : any
;; 当 key 未找到或选项列表不合法时返回的默认值。
;;
;; 返回值
;; ------
;; any
;; 查找到的对应键值，或在未找到时返回 default。
;;
;; 说明
;; ----
;; - 按成对顺序扫描 opts 列表。
;; - 若 key 重复出现，返回首个匹配的键值。
;; - 若 opts 剩余单数个元素（畸形选项），返回 default。

(import (liii check) (liii subprocess process))

(check-set-mode! 'report-failed)

;; key 命中时正确提取下一个参数值
(check (%keyword-value :cwd '(:cwd "/tmp" :env ()) #f) => "/tmp")
(check
  (%keyword-value :env '(:cwd "/tmp" :env (("A" . "B"))) #f)
  =>
  '(("A" . "B"))
) ;check

;; key 未在 opts 列表中时返回 default
(check (%keyword-value :stdin '(:cwd "/tmp") :pipe) => :pipe)
(check (%keyword-value :cwd '() "/default") => "/default")

;; 剩余长度为奇数（末尾单值畸形选项）时回退 default
(check (%keyword-value :timeout '(:timeout) 1000) => 1000)
(check (%keyword-value :cwd '(:flag :cwd "/tmp") "/fallback") => "/fallback")

;; 多组相同 key 时按先来先取规则匹配首个值
(check (%keyword-value :cwd '(:cwd "/first" :cwd "/second") #f) => "/first")

(check-report)
