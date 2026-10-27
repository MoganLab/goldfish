(import (liii check) (liii pi code cli args) (liii pi code core prompt))

(check-set-mode! 'report-failed)

;; build-initial-prompt
;; 组装脚本模式初始提示词与后续消息队列。
(let-values (((prompt rest) (build-initial-prompt #f "" '("message 1" "message 2"))))
  (check prompt => "message 1")
  (check rest => '("message 2")))

(let-values (((prompt rest) (build-initial-prompt "stdin content\n" "" '("first message"))))
  (check prompt => "stdin content\nfirst message")
  (check rest => '()))

(let-values (((prompt rest)
              (build-initial-prompt "stdin content\n"
                                   "<file name=\"/foo\">bar</file>\n"
                                   '("do something" "followup"))))
  (check prompt => "stdin content\n<file name=\"/foo\">bar</file>\ndo something")
  (check rest => '("followup")))

(let-values (((prompt rest)
              (build-initial-prompt #f "<file name=\"/foo\">bar</file>\n" '())))
  (check prompt => "<file name=\"/foo\">bar</file>\n")
  (check rest => '()))

(let-values (((prompt rest) (build-initial-prompt #f "" '())))
  (check prompt => "")
  (check rest => '()))

;; 结合 parse-pi-args 与 build-initial-prompt 进行完整 prompt 组装测试
(let* ((parsed (parse-pi-args '("--model" "gemini-3.8-flash" "--" "-h" "second message")))
       (messages (pi-args-ref parsed 'messages)))
  (let-values (((initial rest)
                (build-initial-prompt "stdin text\n"
                                      "<file name=\"/abs/path\">\nfile content\n</file>\n"
                                      messages)))
    (check initial => "stdin text\n<file name=\"/abs/path\">\nfile content\n</file>\n-h")
    (check rest => '("second message"))))

(check-report)
