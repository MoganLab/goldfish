(import (liii check) (liii string) (liii path) (liii pi code core prompt context))

(check-set-mode! 'report-failed)

;; pi-resolve-prompt-input
;; 解析直接字符串输入或文件引用输入。
(define REPO_ROOT
  (cond ((file-exists? "xmake.lua") ".")
        ((file-exists? "../../xmake.lua") "../..")
        (else ".")))

(define TEST_FILE
  (path->string (path-join (path REPO_ROOT) (path "tools/pi/tests/fixtures/sample.txt"))))

(check (pi-resolve-prompt-input "Hello prompt") => "Hello prompt")
(check (pi-resolve-prompt-input #f) => #f)

(let ((content (pi-resolve-prompt-input TEST_FILE)))
  (check-true (string? content))
  (check-true (string-contains? content "hello sample")))

(check-report)
