;; parse-test-clear-cache
;; 解析 --clear-cache 清理测试缓存目录标志
;;
;; 语法
;; ----
;; (parse-test-clear-cache args)
;;
;; 参数
;; ----
;; args : list?，命令行参数列表（首元素为可执行文件路径）
;;
;; 返回值
;; ------
;; boolean?，出现 --clear-cache 时为 #t

(import (scheme base) (liii check) (liii goldtest-args))

(check-set-mode! 'report-failed)

;; ===== 场景1: 无 --clear-cache 时为 #f =====
(check-false (parse-test-clear-cache '("bin/gf" "test")))
(check-false (parse-test-clear-cache '("bin/gf" "test" "--force" "json")))

;; ===== 场景2: --clear-cache 选项 =====
(check-true (parse-test-clear-cache '("bin/gf" "test" "--clear-cache")))
(check-true (parse-test-clear-cache '("bin/gf" "test" "--clear-cache" "tools/pi/tests/")))

;; ===== 场景3: 与其他选项混用 =====
(check-true
  (parse-test-clear-cache '("bin/gf" "test" "-j" "8" "--clear-cache" "--force")))

(check-report)
