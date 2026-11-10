;; process-file-arguments
;; 处理命令行中的文件参数，展开目录与 @file 引用。
;;
;; 语法
;; ----
;; (process-file-arguments paths)
;;
;; 参数
;; ----
;; paths : list
;; 文件或目录路径列表。
;;
;; 返回值
;; ----
;; list
;; 处理后的文件内容块列表。

(import (liii check) (liii string) (liii pi code cli args))

(check-set-mode! 'report-failed)

;; process-file-arguments
(let* ((test-file (if (file-exists? "liii/pi/code/cli/args.scm")
                    "liii/pi/code/cli/args.scm"
                    "tools/pi/liii/pi/code/cli/args.scm"
                  ) ;if
       ) ;test-file
       (block (process-file-arguments (list test-file)))
      ) ;
  (check-true (string? block))
  (check-true (> (string-length block) 0))
  (check-true (string-contains? block "<file name=\""))
  (check-true (string-contains? block "args.scm"))
  (check-true (string-contains? block "</file>\n"))
) ;let*

;; 空文件列表返回空字符串
(let ((empty-result (process-file-arguments '())))
  (check empty-result => "")
) ;let

;; 文件不存在时报错
(check-catch 'file-error (process-file-arguments '("/nonexistent-file-xyz")))

(check-report)
