(import (liii check) (liii goldfmt stem) (liii goldfmt scan) (srfi srfi-13))

(check-set-mode! 'report-failed)

;; 1. format-stem-string

(define stem-text
  (format-stem-string "(document (chapter* (unquote title)) (quote (a b)))")
) ;define

(check-true (string-contains stem-text "(unquote title)"))
(check-true (string-contains stem-text "(quote (a b))"))
(check-false (string-contains stem-text ",title"))
(check-false (string-contains stem-text "'(a b)"))

;; 2. stem-format-nodes

(define nodes
  (call-with-stem-mode (lambda () (scan-string "(document (title (unquote t)))")))
) ;define

(define formatted-nodes (stem-format-nodes nodes))
(check-true (string-contains formatted-nodes "(unquote t)"))
(check-false (string-contains formatted-nodes ",t"))

(check-report)
