(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-read
;; 执行 read 工具读取指定文件内容。

(define test-file
  (cond ((file-exists? "tools/pi/tests/fixtures/sample.txt")
         "tools/pi/tests/fixtures/sample.txt"
        ) ;
        ((file-exists? "tests/fixtures/sample.txt") "tests/fixtures/sample.txt")
        ((file-exists? "../../fixtures/sample.txt") "../../fixtures/sample.txt")
        (else "tools/pi/tests/fixtures/sample.txt")
  ) ;cond
) ;define

(let ((content (pi-execute-read `(("path" . ,test-file) ("limit" . 2)))))
  (check-true (string? content))
  (check content
    =>
    "hello sample\nline 2\n\n[1 more line in file. Use offset=3 to continue.]"
  ) ;check
) ;let

(let ((err-msg
        (catch 'error
          (lambda () (pi-execute-read `(("path" . ,test-file) ("offset" . 4))))
          (lambda (tag . info) (if (and (pair? info) (pair? (car info))) (caar info) ""))
        ) ;catch
      ) ;err-msg
     ) ;
  (check err-msg => "Offset 4 is beyond end of file (3 lines total)")
) ;let

(check-report)
