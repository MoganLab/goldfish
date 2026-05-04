(import (liii logging))

;; 测试 stdout handler
(display "=== stdout handler ===")
(newline)
(log-set-callback! (make-stdout-handler))
(log-info "This is an info message to stdout")
(log-debug "This is a debug message to stdout")

;; 测试 stderr handler
(display "=== stderr handler ===")
(newline)
(log-set-callback! (make-stderr-handler))
(log-error "This is an error message to stderr")
(log-warning "This is a warning message to stderr")

;; 设置文件 handler，将日志写入当前目录下的 app.log
(display "=== file handler ===")
(newline)
(log-set-file-handler! "app.log")
(log-set-level! DEBUG)

;; 记录不同级别的日志
(log-debug "This is a debug message")
(log-info "Application started")
(log-warning "This is a warning")
(log-error "Something went wrong: %(code)d" :code 404)

;; 带格式化的日志
(log-info "User %(name)s logged in at %(ip)s" :name "Alice" :ip "192.168.1.1")

(display "Logs have been written to app.log")
(newline)
