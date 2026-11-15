;; call-with-input-file
;; 打开文件供输入，并以打开的端口调用过程，过程返回后自动关闭端口。
;;
;; 语法
;; ----
;; (call-with-input-file string proc)
;;
;; 参数
;; ----
;; string : string?
;; 要打开的文件路径。
;;
;; proc : procedure?
;; 接受一个输入端口作为参数的过程。
;;
;; 返回值
;; ----
;; 过程 proc 的返回值。
;;
;; 说明
;; ----
;; 1. 打开指定文件并将其作为参数传递给 proc。
;; 2. 当 proc 返回时，自动关闭该端口。

(import (liii check) (liii path) (liii tempfile) (scheme file))
(check-set-mode! 'report-failed)

(with-temp-dir
  (lambda (dir)
    (let ((test-file (path->string (path-join dir "test-call-with-input-file.txt")))
          (chinese-file (path->string (path-join dir "中文测试.txt")))
         ) ;
      ;; 创建测试文件
      (with-output-to-file test-file (lambda () (display "hello world")))
      ;; 测试 call-with-input-file 基本功能
      (check (call-with-input-file test-file (lambda (port) (read-string 100 port)))
        =>
        "hello world"
      ) ;check
      ;; 测试读取多行
      (with-output-to-file test-file
        (lambda () (display "line1\n") (display "line2"))
      ) ;with-output-to-file
      (check
        (call-with-input-file test-file
          (lambda (port) (list (read-line port) (read-line port)))
        ) ;call-with-input-file
        =>
        '("line1" "line2")
      ) ;check
      ;; 测试中文文件名
      (with-output-to-file chinese-file (lambda () (display "中文内容")))
      (check (call-with-input-file chinese-file (lambda (port) (read-string 100 port)))
        =>
        "中文内容"
      ) ;check
      (delete-file chinese-file)
      ;; 清理
      (delete-file test-file)
    ) ;let
  ) ;lambda
) ;with-temp-dir

(check-report)
