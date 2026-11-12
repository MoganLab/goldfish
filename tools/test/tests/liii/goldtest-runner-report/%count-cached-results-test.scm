;; %count-cached-results
;; 从并行结果列表统计缓存命中次数
;;
;; 语法
;; ----
;; (%count-cached-results raw-results)
;;
;; 参数
;; ----
;; raw-results : list?，元素为 (list file code tag)
;;
;; 返回值
;; ------
;; integer?，tag 为 "cached" 且 code 为 0 的条数

(import (scheme base) (liii check) (liii goldtest-runner-report))

(check-set-mode! 'report-failed)

;; ===== 场景1: 空列表 =====
(check (%count-cached-results '()) => 0)

;; ===== 场景2: 混合结果只统计 cached 且成功 =====
(check
  (%count-cached-results
    (list (list "a.scm" 0 "cached") (list "b.scm" 0 "") (list "c.scm" 0 "cached")))
  => 2)

;; ===== 场景3: 非缓存标记与失败条目不计入 =====
(check
  (%count-cached-results
    (list (list "a.scm" 1 "cached") (list "b.scm" 0 "x")))
  => 0)

;; ===== 场景4: 是过程 =====
(check-true (procedure? %count-cached-results))

(check-report)
