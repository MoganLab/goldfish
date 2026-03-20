(import (liii logging)
        (liii rich-path))

(define log (logging "demo"))
(log :set-path! (path :temp-dir :/ "demo.log" :to-string))

(log :info "Hello, world!")
(log :debug "Hello, world!")
(log :warning "Hello, world!")
(log :error "Hello, world!")
(log :critical "Hello, world!")

(display* "log path: " (log :get-path) "\n")
(display* "log level: " (log :get-level) "\n")
