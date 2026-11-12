;; *test-cache-force*
;; 缓存强制绕过标志（#t 表示忽略缓存，全部真实执行）
;;
;; 语法
;; ----
;; *test-cache-force*
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; boolean?，初始为 #f；由 set-test-cache-force! 修改

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 初始值为 #f =====
(check-false *test-cache-force*)

;; ===== 场景2: setter 修改后经 test-cache-force? 查询联动 =====
;; 注意：R7RS 下 import 绑定是库变量在导入时的拷贝，
;; 库内变量变更须经库导出的查询过程观察
(set-test-cache-force! #t)
(check-true (test-cache-force?))
(set-test-cache-force! #f)
(check-false (test-cache-force?))

;; ===== 场景3: 还原默认，避免影响其他测试 =====
(set-test-cache-force! #f)
(check-false (test-cache-force?))

(check-report)
