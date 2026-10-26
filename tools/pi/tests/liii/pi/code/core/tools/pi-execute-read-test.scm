(import (liii check) (liii path) (liii pi code core tools read) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; pi-execute-read
;; 执行 read 工具读取指定文件内容。

(define (%read-error-msg thunk)
  (catch 'error thunk
    (lambda (tag . info) (if (and (pair? info) (pair? (car info))) (caar info) ""))
  ) ;catch
) ;define

(define (%tmp-file name)
  (path->string (path-join (path-temp-dir) name))
) ;define

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

(check (%read-error-msg
         (lambda () (pi-execute-read `(("path" . ,test-file) ("offset" . 4))))
       ) ;%read-error-msg
  =>
  "Offset 4 is beyond end of file (3 lines total)"
) ;check

;; 测试项1 & 2：标准换行结尾文件的总行数、剩余行数与越界 offset 报告
(let ((sample-file (%tmp-file "pi-sample-1073.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2\n"))
  ) ;pi-execute-write
  ;; limit=1 读取 2 行文件，提示剩余 1 行
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    "line1\n\n[1 more line in file. Use offset=2 to continue.]"
  ) ;check
  ;; 完整读取
  (check (pi-execute-read (list (cons "path" sample-file)))
    =>
    "line1\nline2"
  ) ;check
  ;; offset=2 读取最后 1 行
  (check (pi-execute-read (list (cons "path" sample-file) (cons "offset" 2)))
    =>
    "line2"
  ) ;check
  ;; 越界 offset 报错行数符合实际 (2 lines total)
  (check (%read-error-msg
           (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3))))
         ) ;%read-error-msg
    =>
    "Offset 3 is beyond end of file (2 lines total)"
  ) ;check
  (check (%read-error-msg
           (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 4))))
         ) ;%read-error-msg
    =>
    "Offset 4 is beyond end of file (2 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; 末尾无换行测试
(let ((sample-file (%tmp-file "pi-sample-no-eol.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2"))
  ) ;pi-execute-write
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    "line1\n\n[1 more line in file. Use offset=2 to continue.]"
  ) ;check
  (check-catch 'error
    (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3)))
  ) ;check-catch
  (delete-file sample-file)
) ;let

;; 末尾多换行测试 (3 lines total)
(let ((sample-file (%tmp-file "pi-sample-multi-nl.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2\n\n"))
  ) ;pi-execute-write
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 2)))
    =>
    "line1\nline2\n\n[1 more line in file. Use offset=3 to continue.]"
  ) ;check
  (check (%read-error-msg
           (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 4))))
         ) ;%read-error-msg
    =>
    "Offset 4 is beyond end of file (3 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; 空文件测试 (0 lines total)
(let ((sample-file (%tmp-file "pi-sample-empty.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "")))
  (check (pi-execute-read (list (cons "path" sample-file)))
    =>
    ""
  ) ;check
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    ""
  ) ;check
  (check (%read-error-msg
           (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 1))))
         ) ;%read-error-msg
    =>
    "Offset 1 is beyond end of file (0 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; CRLF 换行格式测试
(let ((sample-file (%tmp-file "pi-sample-crlf.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\r\nline2\r\n"))
  ) ;pi-execute-write
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    "line1\n\n[1 more line in file. Use offset=2 to continue.]"
  ) ;check
  (check (%read-error-msg
           (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3))))
         ) ;%read-error-msg
    =>
    "Offset 3 is beyond end of file (2 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

(check-report)
