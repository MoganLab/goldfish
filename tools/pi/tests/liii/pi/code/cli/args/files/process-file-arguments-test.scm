(import (liii check)
  (liii string)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; process-file-arguments
(let* ((test-file (if (file-exists? "liii/pi/code/cli/args.scm")
                    "liii/pi/code/cli/args.scm"
                    "tools/pi/liii/pi/code/cli/args.scm"))
       (block (process-file-arguments (list test-file))))
  (check-true (string? block))
  (check-true (> (string-length block) 0))
  (check-true (string-contains? block "<file name=\""))
  (check-true (string-contains? block "args.scm"))
  (check-true (string-contains? block "</file>\n")))

;; 空文件列表返回空字符串
(let ((empty-result (process-file-arguments '())))
  (check empty-result => ""))

;; 文件不存在时报错
(check-catch 'file-error (process-file-arguments '("/nonexistent-file-xyz")))

(check-report)
