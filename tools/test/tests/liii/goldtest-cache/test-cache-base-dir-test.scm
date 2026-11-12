;; test-cache-base-dir
;; 测试缓存根目录：~/.cache/goldfish/test/<version>/
;;
;; 语法
;; ----
;; (test-cache-base-dir)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; string?，缓存根目录绝对路径

(import (scheme base) (liii check) (liii string) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 路径含 /goldfish/test/ 段 =====
(define base (test-cache-base-dir))
(check-true (string? base))
(check-true (string-contains base "/.cache/goldfish/test/"))

;; ===== 场景2: 两次调用一致 =====
(check (test-cache-base-dir) => (test-cache-base-dir))

;; ===== 场景3: 是无参过程 =====
(check-true (procedure? test-cache-base-dir))

(check-report)
