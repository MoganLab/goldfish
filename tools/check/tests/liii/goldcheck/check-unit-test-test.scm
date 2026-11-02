;; check-unit-test
;; 检查单个单元测试文件是否符合代码规范（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (check-unit-test file-path)
;;
;; 参数
;; ----
;; file-path : string   测试文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 违规信息列表，合规时返回空列表。

(import (scheme base) (scheme file) (liii check) (liii path) (liii goldcheck))

(check-set-mode! 'report-failed)

;; 1. 规范测试文件无违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-root-good.scm")))
       (content (string-append ";; baz\n;; baz 函数说明。\n;;\n;; 语法\n;; ----\n;; (baz)\n;;\n"
                  ";; 参数\n;; ----\n;; 无\n;;\n;; 返回值\n;; ------\n;; any\n\n"
                  "(import (scheme base) (liii check))\n\n" "(check 1 => 1)\n\n"
                  "(check-report)\n"
                ) ;string-append
       ) ;content
      ) ;
  (call-with-output-file tmp (lambda (p) (write-string content p)))
  (check (check-unit-test tmp) => '())
  (path-unlink (path tmp))
) ;let*

(check-report)
