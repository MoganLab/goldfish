;; check-unit-test-file-content
;; 检查单个测试文件的内容规范（包含顶部文档、文档要素与末尾报告检查）。
;;
;; 语法
;; ----
;; (check-unit-test-file-content file-path rel-path)
;;
;; 参数
;; ----
;; file-path : string   测试文件路径
;; rel-path : string    相对路径（用于违规报错）
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表，符合规范则为空列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii goldcheck unit-test file)
) ;import

(check-set-mode! 'report-failed)

;; 1. 空测试文件违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-empty.scm"))))
  (call-with-output-file tmp (lambda (p) #f))
  (let ((viols (check-unit-test-file-content tmp "test.scm")))
    (check (length viols) => 1)
    (check (list-ref (car viols) 3) => "空测试文件")
  ) ;let
  (path-unlink (path tmp))
) ;let*

;; 2. 文档未置顶违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-doc.scm"))))
  (call-with-output-file tmp
    (lambda (p) (write-string "(import (scheme base))\n\n(check-report)\n" p))
  ) ;call-with-output-file
  (let ((viols (check-unit-test-file-content tmp "test.scm")))
    (check (length viols) => 1)
    (check (list-ref (car viols) 3)
      =>
      "文档必须置于文件最顶部（在 import 之前）"
    ) ;check
  ) ;let
  (path-unlink (path tmp))
) ;let*

(check-report)
