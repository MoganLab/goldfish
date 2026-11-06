;; with-output-to-file
;; 将当前输出端口重定向到指定文件并调用过程。
;;
;; 语法
;; ----
;; (with-output-to-file string thunk)
;;
;; 参数
;; ----
;; string : string?
;; 要打开或创建的文件路径。
;;
;; thunk : procedure?
;; 接受零个参数的过程。
;;
;; 返回值
;; ----
;; 过程 thunk 的返回值。
;;
;; 说明
;; ----
;; 1. 打开指定文件并将其设为当前输出端口。
;; 2. 调用过程 thunk。
;; 3. 在 thunk 返回时恢复原来的输出端口并关闭打开的文件端口。

(import (liii check) (scheme file))
(check-set-mode! 'report-failed)

(define test-file "tests/scheme/file/test-with-output.txt")
;; 清理可能存在的旧文件
(when (file-exists? test-file)
  (delete-file test-file)
) ;when
;; 测试 with-output-to-file
(with-output-to-file test-file (lambda () (display "output to file")))
;; 验证文件内容
(check (call-with-input-file test-file (lambda (port) (read-string 100 port)))
  =>
  "output to file"
) ;check
;; 测试覆盖写入
(with-output-to-file test-file (lambda () (display "new output")))
(check (call-with-input-file test-file (lambda (port) (read-string 100 port)))
  =>
  "new output"
) ;check
;; 测试中文文件名

(define chinese-file "tests/scheme/file/中文输出重定向.txt")
(with-output-to-file chinese-file (lambda () (display "中文输出内容")))
(check (call-with-input-file chinese-file (lambda (port) (read-string 100 port)))
  =>
  "中文输出内容"
) ;check
(delete-file chinese-file)
;; 清理
(delete-file test-file)

(check-report)
