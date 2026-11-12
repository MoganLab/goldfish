;; parse-test-force
;; 解析 --force / --no-cache 强制绕过缓存标志
;;
;; 语法
;; ----
;; (parse-test-force args)
;;
;; 参数
;; ----
;; args : list?，命令行参数列表（首元素为可执行文件路径）
;;
;; 返回值
;; ------
;; boolean?，出现 --force 或 --no-cache 时为 #t

(import (scheme base) (liii check) (liii goldtest-args))

(check-set-mode! 'report-failed)

;; ===== 场景1: 无相关选项时为 #f =====
(check-false (parse-test-force '("bin/gf" "test")))

;; ===== 场景2: --force 选项 =====
(check-true (parse-test-force '("bin/gf" "test" "--force" "json")))
(check-true (parse-test-force '("bin/gf" "test" "--force")))

;; ===== 场景3: --no-cache 选项 =====
(check-true (parse-test-force '("bin/gf" "test" "--no-cache" "tools/pi/tests/")))
(check-true (parse-test-force '("bin/gf" "test" "--no-cache")))

;; ===== 场景4: 与其他选项混用 =====
(check-true (parse-test-force '("bin/gf" "test" "-j" "4" "--force" "json")))
(check-false (parse-test-force '("bin/gf" "test" "-j" "4" "json")))

(check-report)
