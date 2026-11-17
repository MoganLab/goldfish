;; find-test-dependencies
;; 递归解析测试文件、引导文件、resources/ 及 @test-depends 全部依赖
;;
;; 语法
;; ----
;; (find-test-dependencies test-file)
;;
;; 参数
;; ----
;; test-file : string?，测试文件路径
;;
;; 返回值
;; ------
;; list?，按字典序排列的依赖文件路径列表

(import (scheme base) (liii check) (liii list) (liii string) (liii goldtest-cache-deps))

(check-set-mode! 'report-failed)

;; ===== 场景1: 递归收集传递依赖与引导文件 =====
(define deps (find-test-dependencies "tests/liii/goldtest-cache/test-action-key-test.scm"))
(check-true (list? deps))
(check-true (and (member "liii/goldtest-cache.scm" deps string=?) #t))
(check-true (any (lambda (p) (string-ends? p "r7rs/scheme/boot.scm")) deps))
(check-true (any (lambda (p) (string-ends? p "r7rs/scheme/base.scm")) deps))

;; ===== 场景2: 不存在的测试文件返回空列表 =====
(check (find-test-dependencies "nonexistent/foo-test.scm") => '())

;; ===== 场景3: 是过程 =====
(check-true (procedure? find-test-dependencies))

(check-report)
