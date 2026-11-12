;; tools-test-path?
;; 判断测试文件路径是否属于 tools/ 目录（缓存仅对 tools 目录生效）
;;
;; 语法
;; ----
;; (tools-test-path? path)
;;
;; 参数
;; ----
;; path : string?，测试文件路径
;;
;; 返回值
;; ------
;; boolean?，路径以 tools/ 开头（或当前工作目录位于 tools/ 下）时为 #t；
;; library/、goldfish/、r7rs/ 开头的路径恒为 #f

(import (scheme base) (liii check) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: tools/ 前缀路径判定为启用 =====
(check-true (tools-test-path? "tools/pi/tests/liii/pi/main-test.scm"))
(check-true (tools-test-path? "tools/test/tests/liii/goldtest-cache/tools-test-path-p-test.scm"))
(check-true (tools-test-path? "tools/fmt/tests/liii/goldfmt/main-test.scm"))

;; ===== 场景2: 非 string 输入判定为禁用 =====
(check-false (tools-test-path? #f))
(check-false (tools-test-path? '()))
(check-false (tools-test-path? 100))

;; ===== 场景3: C++ 绑定板块路径判定为禁用 =====
(check-false (tools-test-path? "library/tests/liii/json-test.scm"))
(check-false (tools-test-path? "goldfish/tests/liii/list-test.scm"))
(check-false (tools-test-path? "r7rs/tests/scheme/base-test.scm"))

;; ===== 场景4: 域内相对路径（cwd 含 /tools/ 时判定为启用） =====
;; 本测试在 tools/test 域内运行，tests/ 前缀同样属于 tools 测试
(check-true (tools-test-path? "tests/liii/goldtest-cache/main-test.scm"))

(check-report)
