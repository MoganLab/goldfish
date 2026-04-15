(import (liii check)
        (liii goldfmt-record))

;; good-env?
;; 检查一棵 env 树是否满足基本结构约束。
;;
;; 语法
;; ----
;; (good-env? env)
;;
;; 参数
;; ----
;; env : env?
;; 待检查的环境节点。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示 env 树满足约束；否则返回 #f。
;;
;; 说明
;; ----
;; 当前检查包括：
;; 1. 子 env 的 depth 必须等于父 env 的 depth + 1
;; 2. depth 为 0 的 env，indent 必须是 0 或 -1
;; 3. 非根 env 的 indent 必须大于或等于父 env 的 indent

;; 测试 1: 深度为 0，缩进为 0 - 合法
(check (good-env? (make-env :tag-name "root" :depth 0 :indent 0)) => #t)

;; 测试 2: 深度为 0，缩进为 -1 - 合法（缩进未确认）
(check (good-env? (make-env :tag-name "root" :depth 0 :indent -1)) => #t)

;; 测试 3: 深度为 0，缩进为 2 - 非法（深度0的节点缩进必须为0）
(check (good-env? (make-env :tag-name "root" :depth 0 :indent 2)) => #f)

;; 测试 4: 合法的嵌套环境 - 子环境深度正确
(let ((child (make-env :tag-name "child" :depth 1 :indent 0))
      (parent (make-env :tag-name "parent" :depth 0 :indent 0)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child))) => #t))

;; 测试 5: 非法的嵌套环境 - 子环境深度错误（应为1，实际为2）
(let ((child (make-env :tag-name "child" :depth 2 :indent 0))
      (parent (make-env :tag-name "parent" :depth 0 :indent 0)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child))) => #f))

;; 测试 6: 合法的嵌套环境 - 子环境缩进等于父环境
(let ((child (make-env :tag-name "child" :depth 1 :indent 0))
      (parent (make-env :tag-name "parent" :depth 0 :indent 0)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child))) => #t))

;; 测试 7: 合法的嵌套环境 - 子环境缩进大于父环境
(let ((child (make-env :tag-name "child" :depth 1 :indent 2))
      (parent (make-env :tag-name "parent" :depth 0 :indent 0)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child))) => #t))

;; 测试 8: 非法的嵌套环境 - 子环境缩进小于父环境
(let ((child (make-env :tag-name "child" :depth 1 :indent -1))
      (parent (make-env :tag-name "parent" :depth 0 :indent 0)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child))) => #f))

;; 测试 9: 多层嵌套 - 合法
(let ((grandchild (make-env :tag-name "grandchild" :depth 2 :indent 2))
      (child (make-env :tag-name "child" :depth 1 :indent 0))
      (parent (make-env :tag-name "parent" :depth 0 :indent 0)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector (make-env :tag-name "child"
                                                           :depth 1
                                                           :indent 0
                                                           :children (vector grandchild))))) => #t))

;; 测试 10: 空标签 - 合法
(check (good-env? (make-env :tag-name "" :depth 0 :indent 0)) => #t)

;; 测试 11: 多个子环境 - 全部合法
(let ((child1 (make-env :tag-name "child1" :depth 1 :indent 0))
      (child2 (make-env :tag-name "child2" :depth 1 :indent 2)))
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child1 child2))) => #t))

;; 测试 12: 多个子环境 - 其中一个非法
(let ((child1 (make-env :tag-name "child1" :depth 1 :indent 0))
      (child2 (make-env :tag-name "child2" :depth 2 :indent 2))) ; 深度错误
  (check (good-env? (make-env :tag-name "parent"
                               :depth 0
                               :indent 0
                               :children (vector child1 child2))) => #f))

(check-report)
