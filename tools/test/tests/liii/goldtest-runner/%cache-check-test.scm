;; %cache-check
;; 检查测试文件缓存是否命中（返回命中 Key 或 #f）
;;
;; 语法
;; ----
;; (%cache-check test-file . maybe-force)
;;
;; 参数
;; ----
;; test-file : string?，测试文件路径
;; maybe-force : 可选 boolean?，强制绕过标志（并行 worker 场景参数化传入）
;;
;; 返回值
;; ------
;; 命中时返回 Action Key 字符串，未命中/禁用/强制时返回 #f

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 非 tools 路径（library/ 等）恒不缓存 =====
(check-false (%cache-check "library/tests/liii/json-test.scm"))
(check-false (%cache-check "r7rs/tests/scheme/base-test.scm"))

;; ===== 场景2: force 标志参数化时直接返回 #f =====
(check-false (%cache-check "tests/liii/goldtest-args/main-test.scm" #t))

;; ===== 场景3: 库级 force 标志同样绕过 =====
(set-test-cache-force! #t)
(check-false (%cache-check "tests/liii/goldtest-args/main-test.scm"))
(set-test-cache-force! #f)

;; ===== 场景4: 不存在的文件不命中且不抛错 =====
;; 注意：使用本测试专属的不存在路径，避免与其他并行子进程的缓存写入
;; 形成共享状态竞态
(check-false (%cache-check "nonexistent/cache-check-probe-only.scm"))

;; ===== 场景5: 是过程 =====
(check-true (procedure? %cache-check))

(check-report)
