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

;; 测试项1 & 2：标准换行结尾文件的总行数、剩余行数与越界 offset 报告
(let ((sample-file "tests/liii/pi-sample-1073.txt"))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2\n"))
  ) ;pi-execute-write
  ;; limit=1 读取 2 行文件，提示剩余 1 行
  (let ((res (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))))
    (check res => "line1\n\n[1 more line in file. Use offset=2 to continue.]")
  ) ;let
  ;; 完整读取
  (let ((res (pi-execute-read (list (cons "path" sample-file)))))
    (check res => "line1\nline2")
  ) ;let
  ;; offset=2 读取最后 1 行
  (let ((res (pi-execute-read (list (cons "path" sample-file) (cons "offset" 2)))))
    (check res => "line2")
  ) ;let
  ;; 越界 offset=3 报错行数符合实际 (2 lines total)
  (let ((err-msg
          (catch 'error
            (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3))))
            (lambda (tag . info) (if (and (pair? info) (pair? (car info))) (caar info) ""))
          ) ;catch
        ) ;err-msg
       ) ;
    (check err-msg => "Offset 3 is beyond end of file (2 lines total)")
  ) ;let
  ;; 越界 offset=4 报错行数符合实际 (2 lines total)
  (let ((err-msg
          (catch 'error
            (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 4))))
            (lambda (tag . info) (if (and (pair? info) (pair? (car info))) (caar info) ""))
          ) ;catch
        ) ;err-msg
       ) ;
    (check err-msg => "Offset 4 is beyond end of file (2 lines total)")
  ) ;let
  (delete-file sample-file)
) ;let

;; 末尾无换行测试
(let ((sample-file "tests/liii/pi-sample-no-eol.txt"))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2"))
  ) ;pi-execute-write
  (let ((res (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))))
    (check res => "line1\n\n[1 more line in file. Use offset=2 to continue.]")
  ) ;let
  (check-catch 'error
    (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3)))
  ) ;check-catch
  (delete-file sample-file)
) ;let

;; 末尾多换行测试 (3 lines total)
(let ((sample-file "tests/liii/pi-sample-multi-nl.txt"))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2\n\n"))
  ) ;pi-execute-write
  (let ((res (pi-execute-read (list (cons "path" sample-file) (cons "limit" 2)))))
    (check res
      =>
      "line1\nline2\n\n[1 more line in file. Use offset=3 to continue.]"
    ) ;check
  ) ;let
  (let ((err-msg
          (catch 'error
            (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 4))))
            (lambda (tag . info) (if (pair? info) (caar info) ""))
          ) ;catch
        ) ;err-msg
       ) ;
    (check err-msg => "Offset 4 is beyond end of file (3 lines total)")
  ) ;let
  (delete-file sample-file)
) ;let

;; 空文件测试 (0 lines total)
(let ((sample-file "tests/liii/pi-sample-empty.txt"))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "")))
  (let ((res (pi-execute-read (list (cons "path" sample-file)))))
    (check res => "")
  ) ;let
  (let ((res (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))))
    (check res => "")
  ) ;let
  (let ((err-msg
          (catch 'error
            (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 1))))
            (lambda (tag . info) (if (pair? info) (caar info) ""))
          ) ;catch
        ) ;err-msg
       ) ;
    (check err-msg => "Offset 1 is beyond end of file (0 lines total)")
  ) ;let
  (delete-file sample-file)
) ;let

;; CRLF 换行格式测试
(let ((sample-file "tests/liii/pi-sample-crlf.txt"))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\r\nline2\r\n"))
  ) ;pi-execute-write
  (let ((res (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))))
    (check res => "line1\n\n[1 more line in file. Use offset=2 to continue.]")
  ) ;let
  (let ((err-msg
          (catch 'error
            (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3))))
            (lambda (tag . info) (if (pair? info) (caar info) ""))
          ) ;catch
        ) ;err-msg
       ) ;
    (check err-msg => "Offset 3 is beyond end of file (2 lines total)")
  ) ;let
  (delete-file sample-file)
) ;let

(check-report)
