;; find-tools-dependencies
;; 递归解析测试文件及其依赖引用的 tools 内部 .scm 文件
;;
;; 语法
;; ----
;; (find-tools-dependencies test-file)
;;
;; 参数
;; ----
;; test-file : string?，测试文件路径
;;
;; 返回值
;; ------
;; list?，按字母序排列的依赖文件路径列表

(import (scheme base) (liii check) (liii sort) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 递归依赖包含被测库源文件且按字母序 =====
(define deps (find-tools-dependencies "tests/liii/goldtest-cache/tools-test-path-p-test.scm"))
(check-true (list? deps))
(check-true (and (member "liii/goldtest-cache.scm" deps) #t))
(check-true (equal? deps (list-sort string<? deps)))

;; ===== 场景2: 不存在的测试文件返回空列表 =====
(check (find-tools-dependencies "nonexistent/test.scm") => '())

;; ===== 场景3: 是过程 =====
(check-true (procedure? find-tools-dependencies))

(check-report)
