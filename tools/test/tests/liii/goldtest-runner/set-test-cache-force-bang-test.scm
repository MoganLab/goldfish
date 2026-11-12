;; set-test-cache-force!
;; 设置缓存强制绕过标志（库内 mutable 变量的合法修改入口）
;;
;; 语法
;; ----
;; (set-test-cache-force! flag)
;;
;; 参数
;; ----
;; flag : boolean? 或任意值（非 #f 归一化为 #t）
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 设置与查询往返一致 =====
(set-test-cache-force! #f)
(check-false (test-cache-force?))
(set-test-cache-force! #t)
(check-true (test-cache-force?))

;; ===== 场景2: 非布尔输入归一化 =====
(set-test-cache-force! 'anything)
(check-true (test-cache-force?))
(set-test-cache-force! #f)
(check-false (test-cache-force?))

;; ===== 场景3: 是过程且可多次调用 =====
(check-true (procedure? set-test-cache-force!))
(set-test-cache-force! #t)
(set-test-cache-force! #t)
(check-true (test-cache-force?))
(set-test-cache-force! #f)

(check-report)
