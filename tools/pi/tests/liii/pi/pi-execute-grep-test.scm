(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-grep
;; 执行 grep 工具按正则表达式或字面量搜索文件内容。
(define test-file
  (cond
    ((file-exists? "tools/pi/tests/fixtures/sample.txt")
     "tools/pi/tests/fixtures/sample.txt")
    ((file-exists? "tests/fixtures/sample.txt")
     "tests/fixtures/sample.txt")
    ((file-exists? "../../fixtures/sample.txt")
     "../../fixtures/sample.txt")
    (else "tools/pi/tests/fixtures/sample.txt")
  ) ;cond
) ;define

(let ((out (pi-execute-grep `(("pattern" . "sample") ("path" . ,test-file)))))
  (check-true (string? out)))

(check-report)
