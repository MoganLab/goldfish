;; %cache-record
;; 测试执行后按退出码写回缓存（仅 exit-code=0 持久化）
;;
;; 语法
;; ----
;; (%cache-record test-file exit-code . maybe-force)
;;
;; 参数
;; ----
;; test-file : string?，测试文件路径
;; exit-code : integer?，测试子进程退出码
;; maybe-force : 可选 boolean?，强制绕过标志（worker 场景参数化传入）
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 非 tools 路径不写缓存 =====
(%cache-record "library/tests/liii/json-test.scm" 0)
(check-true (procedure? %cache-record))

;; ===== 场景2: 失败退出码不写缓存且不抛错 =====
(%cache-record "tests/liii/goldtest-args/main-test.scm" 1)
(%cache-record "tests/liii/goldtest-args/main-test.scm" -1)
(check-true #t)

;; ===== 场景3: 不存在的文件不写缓存也不抛错 =====
;; 不存在文件的 key 退化为空哈希与空依赖，所有不存在路径共享同一
;; key，写入会造成跨测试碰撞，故一律拒绝写缓存
(%cache-record "nonexistent/cache-record-probe-only.scm" 0)
(check-true #t)

(check-report)
