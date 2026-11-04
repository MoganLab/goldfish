;; find-node-rules-path
;; 查找当前生效的 node-rules.json 文件路径。
;;
;; 语法
;; ----
;; (find-node-rules-path)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; string?
;; node-rules.json 文件路径，找不到时返回默认的 "node-rules.json"。
;;
;; 说明
;; ----
;; 优先使用 `find-node-rules-paths` 找到的第一个路径；
;; 若没有找到，则回退到可读的默认候选路径或字面量 "node-rules.json"。

(import (liii check) (liii goldfmt rule) (liii string))

(check-set-mode! 'report-failed)

;; 返回值一定是字符串
(check (string? (find-node-rules-path)) => #t)

;; 若找到了路径，则以 node-rules.json 结尾
(let ((path (find-node-rules-path)))
  (check (string-ends? path "node-rules.json") => #t)
) ;let

(check-report)
