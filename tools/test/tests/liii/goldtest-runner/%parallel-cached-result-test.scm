;; %parallel-cached-result
;; 并行 worker 中缓存命中结果的处置：打印 PASS (cached) 并返回标记结果
;;
;; 语法
;; ----
;; (%parallel-cached-result idx total test-file)
;;
;; 参数
;; ----
;; idx : integer?，任务序号（0 起）
;; total : integer?，任务总数
;; test-file : string?，测试文件路径
;;
;; 返回值
;; ------
;; (list test-file 0 "cached")，供 %count-cached-results 统计

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 返回结构 (file 0 "cached") =====
(define r (%parallel-cached-result 0 1 "tests/liii/goldtest/main-test.scm"))
(check r => (list "tests/liii/goldtest/main-test.scm" 0 "cached"))

;; ===== 场景2: 序号与总数正确渲染（1 起） =====
(check (%parallel-cached-result 4 10 "x.scm")
  => (list "x.scm" 0 "cached"))

;; ===== 场景3: 是过程 =====
(check-true (procedure? %parallel-cached-result))

(check-report)
