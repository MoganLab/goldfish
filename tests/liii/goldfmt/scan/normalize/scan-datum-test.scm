(import (liii check))
(import (liii goldfmt scan normalize)
        (liii goldfmt record))

(check-set-mode! 'report-failed)

;; scan-datum
;; 将归一化后的 datum 扫描为 formatter 使用的 node（atom 或 env）。
;;
;; 语法
;; ----
;; (scan-datum datum depth)
;;
;; 参数
;; ----
;; datum : any?
;; 已归一化的 datum。
;; depth : number?
;; 当前嵌套深度。
;;
;; 返回值
;; ------
;; atom? 或 env?
;; 原子值返回 atom，list 返回 env。
;;
;; 说明
;; ----
;; 1. 深度信息记录在 node 的 depth 字段
;; 2. 首元素为 symbol 的 list 生成带 tag-name 的 env

;; 测试：number 扫描为 atom
(let ((node (scan-datum 42 0)))
  (check (atom? node) => #t)
  (check (atom-value node) => 42)
  (check (atom-depth node) => 0)
) ;let

;; 测试：嵌套深度传递到 atom
(let ((node (scan-datum 'x 3)))
  (check (atom-depth node) => 3)
) ;let

;; 测试：list 扫描为带 tag-name 的 env
(let ((node (scan-datum '(+ 1 2) 0)))
  (check (env? node) => #t)
  (check (env-tag-name node) => "+")
  (check (vector-length (env-children node)) => 2)
  (check (env-depth node) => 0)
) ;let

;; 测试：首元素不是 symbol 时 tag-name 为空
(let ((node (scan-datum '(1 2 3) 0)))
  (check (env? node) => #t)
  (check (env-tag-name node) => "")
) ;let

;; 测试：子节点深度为父深度加一
(let ((node (scan-datum '(+ 1 2) 0)))
  (let ((child (vector-ref (env-children node) 0)))
    (check (atom-depth child) => 1)
  ) ;let
) ;let

(check-report)
