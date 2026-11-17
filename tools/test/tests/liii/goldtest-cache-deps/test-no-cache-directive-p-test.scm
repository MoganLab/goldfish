;; test-no-cache-directive?
;; 检测测试文件头部是否声明了 @test-no-cache 指令
;;
;; 语法
;; ----
;; (test-no-cache-directive? file-path)
;;
;; 参数
;; ----
;; file-path : string?，测试文件路径
;;
;; 返回值
;; ------
;; boolean?，声明了 ; @test-no-cache 时返回 #t，否则 #f

(import (scheme base) (scheme file) (liii check) (liii os) (liii path) (liii goldtest-cache-deps))

(check-set-mode! 'report-failed)

;; ===== 场景1: 普通测试文件未声明 @test-no-cache =====
(check-false (test-no-cache-directive? "tests/liii/goldtest-cache-deps/extract-file-imports-test.scm"))

;; ===== 场景2: 声明了 ; @test-no-cache 的文件返回 #t =====
(define tmp-file
  (path->string (path-join (path-temp-dir) "gf-test-no-cache-probe.scm")))
(when (file-exists? tmp-file) (delete-file tmp-file))
(call-with-output-file tmp-file
  (lambda (p)
    (display "; @test-no-cache\n(import (scheme base))\n" p)))
(check-true (test-no-cache-directive? tmp-file))
(when (file-exists? tmp-file) (delete-file tmp-file))

;; ===== 场景3: 不存在文件返回 #f =====
(check-false (test-no-cache-directive? "nonexistent/probe.scm"))

(check-report)
