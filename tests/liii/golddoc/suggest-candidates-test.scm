;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
) ;import

(check-set-mode! 'report-failed)

;; suggest-candidates
;; 在候选函数集合中返回编辑距离阈值内的所有匹配项。
;;
;; 语法
;; ----
;; (suggest-candidates query candidates)
;;
;; 参数
;; ----
;; query : string?
;; candidates : list?
;;
;; 返回值
;; ----
;; list?
;; 如果存在前缀匹配，则只返回所有前缀匹配的候选函数名列表；
;; 否则返回所有编辑距离小于等于 `2` 的候选函数名列表。
;;
;; 描述
;; ----
;; 结果排序规则为：
;; 1. 先去重；
;; 2. 与查询完全相等的候选不会出现在结果中；
;; 3. 如果存在前缀匹配，只返回前缀匹配，并按函数名字典序升序；
;; 4. 如果不存在前缀匹配，再按编辑距离升序；
;; 5. 距离相同时按函数名字典序升序。

(check (suggest-candidates "string-spl"
                           '("string-spilt"
                             "string-split"
                             "string-splat"
                             "string-split"
                             "hash-table"))
  => '("string-splat" "string-split")
) ;check

(check (suggest-candidates "string-splst"
                           '("string-spilt"
                             "string-split"
                             "string-splat"
                             "string-split"
                             "hash-table"))
  => '("string-splat" "string-split" "string-spilt")
) ;check

(check (suggest-candidates "string-split"
                           '("string-split" "string-spilt" "string-splat"))
  => '("string-splat" "string-spilt")
) ;check

(check (suggest-candidates "string-split"
                           '("string-split" "string-splits" "string-spilt"))
  => '("string-splits")
) ;check

(check (suggest-candidates "string-splst" '("hash-table" "vector-map"))
  => '()
) ;check

(check-report)
