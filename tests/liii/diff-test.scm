;; (liii diff) 模块函数分类索引与集成测试
;;
;; diff 模块提供纯内存的文本行差异比对与标准统一差异（Unified Diff）生成功能，
;; 对标 Python 官方标准库 difflib，支持多 Hunk 自动聚合与序列相似度计算。

;; ==== 常见用法示例 ====
(import (liii check) (liii diff))

(check-set-mode! 'report-failed)

;; 示例1：文本行统一差异生成
(let ((a '("print('hello')")) (b '("print('hello world')")))
  (unified-diff a b "old.py" "new.py")
) ;let

;; 示例2：字符串便捷差异比对
(unified-diff-string "foo\nbar\n" "foo\nbar2\n")

;; 示例3：序列相似度评分
(diff-ratio "apple" "apply")

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/diff "unified-diff"
;;   bin/gf doc liii/diff "unified-diff-string"
;;   bin/gf doc liii/diff "diff-ratio"

;; ==== 函数分类索引 ====

;; 一、统一差异生成
;; 用于生成符合标准统一差异规范的函数
;;   unified-diff         - 生成行列表标准统一差异补丁行列表
;;   unified-diff-string  - 生成多行字符串标准统一差异补丁字符串

;; 二、序列相似度
;; 用于计算序列相似度的函数
;;   diff-ratio           - 计算两个序列的相似度得分（0.0 ~ 1.0）

;; ==== 集成测试用例 ====

;; 集成测试1：完整代码补丁生成验证
(let* ((old-lines '("def add(a, b):" "    return a + b" "" "def sub(a, b):"
                    "    return a - b")
       ) ;old-lines
       (new-lines '("def add(a, b):" "    # addition" "    return a + b" ""
                    "def sub(a, b):" "    # subtraction" "    return a - b")
       ) ;new-lines
       (patch-lines (unified-diff old-lines new-lines "calc_old.py" "calc_new.py" "" "" 1)
       ) ;patch-lines
      ) ;
  (check (car patch-lines) => "--- calc_old.py")
  (check (cadr patch-lines) => "+++ calc_new.py")
  (check-true (> (length patch-lines) 2))
) ;let*

;; 集成测试2：字符串差异往返
(let* ((from-str "line 1\nline 2\nline 3\n")
       (to-str "line 1\nline 2 modified\nline 3\n")
       (patch (unified-diff-string from-str to-str))
      ) ;
  (check-true (string? patch))
  (check-true (> (string-length patch) 0))
) ;let*

;; 集成测试3：零差异一致性
(check (unified-diff '("alpha" "beta") '("alpha" "beta")) => '())
(check (unified-diff-string "alpha\nbeta\n" "alpha\nbeta\n") => "")
(check (diff-ratio "alpha" "alpha") => 1.0)

(check-report)
