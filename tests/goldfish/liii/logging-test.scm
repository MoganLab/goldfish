(import (liii check)
        (liii logging)
        (liii string)
        (liii lang)
        (liii rich-path)
        (liii error)
) ;import

;; Test set-path! with path object support
(let ((log (logging "test-path-support")))
  ;; Test with string path
  (log :set-path! "/tmp/test.log")
  (check (log :get-log-path) => "/tmp/test.log")
  ;; Test with invalid input should throw type-error
  (check-catch 'type-error ((logging "app") :set-path! 123))
  ;; Test with path object
  (let ((p (path :temp-dir :/ "test-path-object.log")))
    (log :set-path! p)
    (check (log :get-log-path) => (p :to-string))
  ) ;let
) ;let

(check-catch 'type-error ((logging "app") :set-level! "invalid level"))
(check-catch 'value-error ((logging "app") :set-level! 60))
(let* ((logging-get-rich-level (logging "get-rich-level")))
 (logging-get-rich-level :set-level! ($ 50))
 (check (logging-get-rich-level :get-level) => "CRITICAL")
) ;let*

;; Test @apply: Verify that the same logger instance is returned for the same name
(let ((logger1 (logging "test-module"))
      (logger2 (logging "test-module")))
  (check-true (eq? logger1 logger2))
) ;let

;; Test @apply: Verify that different logger instances are returned for different names
(let ((logger1 (logging "module-a"))
      (logger2 (logging "module-b")))
  (check-false (eq? logger1 logger2))
) ;let

(check ((logging "app") :get-level) => "WARNING")
(let* ((logging-get-level (logging "app-get-level")))
  (logging-get-level :set-level! 50)
  (check (logging-get-level :get-level) => "CRITICAL")
) ;let*

(check-false ((logging "app") :debug?))

(check-false ((logging "app") :info?))

(check-true ((logging "app") :warning?))

(check-true ((logging "app") :error?))

(check-true ((logging "app") :critical?))

;; Test logging with rich-string messages
(let ((log (logging "rich-string-test")))
  (log :set-level! 10) ;; DEBUG level
  
  ;; Test using $ to create a rich-string and logging it
  (define log-output (log :info ($ "User ID: " :+ 12345 :+ " logged in from " :+ "192.168.1.1")))
  (check-true (string-contains log-output "User ID: 12345 logged in from 192.168.1.1"))
  
  ;; Test for multi-parameters
  (define log-output2 (log :info "User ID: " "12345" " logged in from " "192.168.1.1"))
  (check-true (string-contains log-output2 "User ID: 12345 logged in from 192.168.1.1"))
  
  ;; Test with Unicode characters in rich-string
  (define unicode-msg ($ "用户: " :+ "admin" :+ " 登录成功 ✓"))
  (define log-output3 (log :error unicode-msg))
  (check-true (string-contains log-output3 " 登录成功 ✓")) 
) ;let

;; Test that debug logging doesn't happen when level is too high
(let ((log (logging "high-level")))
  (log :set-level! 30) ;; WARNING level
  (check-false (log :debug?))
  (check-false (log :info?))
  (check-true (log :warning?))
  (check-true (log :error?))
  (check-true (log :critical?)) 
  
  ;; These shouldn't produce output
  (check (log :debug "This debug message shouldn't appear") => #<unspecified>)
  (check (log :info "This info message shouldn't appear") => #<unspecified>)
  
  ;; These should produce output
  (check-true (string-contains (log :warning "This warning should appear") "This warning should appear"))
  (check-true (string-contains (log :error "This error should appear") "This error should appear"))
  (check-true (string-contains (log :critical "This critical message should appear") "This critical message should appear"))
) ;let

(check-report)
