;; %run-one-parallel-test
;; 单个并行任务体：先查缓存，命中重放 PASS (cached)，否则拉起子进程执行
;;
;; 语法
;; ----
;; (%run-one-parallel-test pair total tmo cache-force)
;;
;; 参数
;; ----
;; pair : (list idx test-file)，索引化任务
;; total : integer?，任务总数
;; tmo : integer?，单文件超时秒数
;; cache-force : boolean?，强制绕过缓存标志
;;
;; 返回值
;; ------
;; (list test-file exit-code tag)，缓存命中时 tag 为 "cached"

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 强制模式真实执行本文件（真实存在的 tools 测试） =====
;; cache-force=#t 时绕过缓存拉起子进程，结果 tag 为空串
(define r1 (%run-one-parallel-test (list 0 "tests/liii/goldtest/main-test.scm") 1 30 #t))
(check-true (and (list? r1) (= (length r1) 3) (zero? (cadr r1))))
(check (caddr r1) => "")

;; ===== 场景2: 同一文件预先写入缓存后非强制模式重放 cached =====
;; 先真实执行写缓存
(%run-one-parallel-test (list 0 "tests/liii/goldtest/main-test.scm") 1 30 #t)
(define r2 (%run-one-parallel-test (list 0 "tests/liii/goldtest/main-test.scm") 1 30 #f))
(check-true (and (list? r2) (zero? (cadr r2))))
(check (caddr r2) => "cached")

;; ===== 场景3: 是过程 =====
(check-true (procedure? %run-one-parallel-test))

(check-report)
