;; parse-test-verbose
;; 解析 -V / --verbose 失败输出不截断标志
;;
;; 语法
;; ----
;; (parse-test-verbose args)
;;
;; 参数
;; ----
;; args : list?，命令行参数列表（首元素为可执行文件路径）
;;
;; 返回值
;; ------
;; boolean?，出现 --verbose 或 -V 时为 #t

(import (scheme base) (liii check) (liii goldtest-args))

(check-set-mode! 'report-failed)

;; ===== 场景1: 无相关选项时为 #f =====
(check-false (parse-test-verbose '("bin/gf" "test")))

;; ===== 场景2: --verbose 选项 =====
(check-true (parse-test-verbose '("bin/gf" "test" "--verbose" "json")))
(check-true (parse-test-verbose '("bin/gf" "test" "--verbose")))

;; ===== 场景3: -V 短选项 =====
(check-true (parse-test-verbose '("bin/gf" "test" "-V" "tools/pi/tests/")))
(check-true (parse-test-verbose '("bin/gf" "test" "-V")))

;; ===== 场景4: 与其他选项组合 =====
(check-true (parse-test-verbose '("bin/gf" "test" "--force" "-V" "string")))
(check-false (parse-test-verbose '("bin/gf" "test" "--force" "string")))

(check-report)
