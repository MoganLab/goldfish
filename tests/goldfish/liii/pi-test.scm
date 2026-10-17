(import (liii check)
        (liii pi))

(check-set-mode! 'report-failed)

;; resolve-app-mode
(check (resolve-app-mode "json" #f #t #t) => 'json)
(check (resolve-app-mode 'json #f #t #t) => 'json)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode #f #t #t #t) => 'print)
(check (resolve-app-mode #f #f #f #t) => 'print)
(check (resolve-app-mode #f #f #t #f) => 'print)
(check (resolve-app-mode #f #f #t #t) => 'print)

;; format-file-block
(check (format-file-block "/path/to/foo.txt" "hello world")
  => "<file name=\"/path/to/foo.txt\">\nhello world\n</file>\n")
(check (format-file-block "/path/to/bar.txt" "line 1\nline 2")
  => "<file name=\"/path/to/bar.txt\">\nline 1\nline 2\n</file>\n")

;; build-initial-prompt:
;; parts = [stdin?, file-text?, first-message] 无分隔符直接 join
;; 1. 只有 message
(let-values (((prompt rest) (build-initial-prompt #f "" '("message 1" "message 2"))))
  (check prompt => "message 1")
  (check rest => '("message 2")))

;; 2. stdin + message
(let-values (((prompt rest) (build-initial-prompt "stdin content\n" "" '("first message"))))
  (check prompt => "stdin content\nfirst message")
  (check rest => '()))

;; 3. stdin + file + message
(let-values (((prompt rest)
              (build-initial-prompt "stdin content\n"
                                   "<file name=\"/foo\">bar</file>\n"
                                   '("do something" "followup"))))
  (check prompt => "stdin content\n<file name=\"/foo\">bar</file>\ndo something")
  (check rest => '("followup")))

;; 4. 只有 file-text，无 messages
(let-values (((prompt rest)
              (build-initial-prompt #f "<file name=\"/foo\">bar</file>\n" '())))
  (check prompt => "<file name=\"/foo\">bar</file>\n")
  (check rest => '()))

;; 5. 全部为空
(let-values (((prompt rest) (build-initial-prompt #f "" '())))
  (check prompt => "")
  (check rest => '()))

(check-report)
