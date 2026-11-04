;; find-node-rules-paths
;; 在 load-path 中查找所有可用的 node-rules.json 文件路径。
;;
;; 语法
;; ----
;; (find-node-rules-paths)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; list?
;; node-rules.json 文件路径组成的列表，找不到时为空列表。
;;
;; 说明
;; ----
;; 只在 load-path 中与当前可执行文件具有公共祖先目录的条目里查找，
;; 每个条目优先检查自身目录，其次检查其父目录。

(import (liii check) (liii goldfmt rule) (liii string))

(check-set-mode! 'report-failed)

;; 返回值一定是列表
(check (list? (find-node-rules-paths)) => #t)

;; 若找到了路径，则每个路径都是字符串且以 node-rules.json 结尾
(let ((paths (find-node-rules-paths)))
  (for-each (lambda (p)
              (check (string? p) => #t)
              (check (string-ends? p "node-rules.json") => #t)
            ) ;lambda
    paths
  ) ;for-each
) ;let

(check-report)
