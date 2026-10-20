(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-model-metadata-record-test.scm
;; 验证 pi-model-metadata record 构造器、谓词与访问器，以及对 assoc 的平滑兼容。

(define test-meta
  (pi-model-metadata "gemini-3.8-flash"
                     "Gemini 3.8 Flash"
                     "google"
                     "https://generativelanguage.googleapis.com/v1beta"
                     1048576
                     65536
                     #t))

(check-true (pi-model-metadata? test-meta))
(check (pi-model-metadata-id test-meta) => "gemini-3.8-flash")
(check (pi-model-metadata-name test-meta) => "Gemini 3.8 Flash")
(check (pi-model-metadata-provider test-meta) => "google")
(check (pi-model-metadata-base-url test-meta) => "https://generativelanguage.googleapis.com/v1beta")
(check (pi-model-metadata-context-window test-meta) => 1048576)
(check (pi-model-metadata-max-tokens test-meta) => 65536)
(check (pi-model-metadata-matched? test-meta) => #t)

;; assoc 兼容访问 model-metadata record
(check (cdr (assoc 'id test-meta)) => "gemini-3.8-flash")
(check (cdr (assoc "id" test-meta)) => "gemini-3.8-flash")
(check (cdr (assoc 'name test-meta)) => "Gemini 3.8 Flash")
(check (cdr (assoc "name" test-meta)) => "Gemini 3.8 Flash")
(check (cdr (assoc 'provider test-meta)) => "google")
(check (cdr (assoc "provider" test-meta)) => "google")
(check (cdr (assoc 'base-url test-meta)) => "https://generativelanguage.googleapis.com/v1beta")
(check (cdr (assoc "base-url" test-meta)) => "https://generativelanguage.googleapis.com/v1beta")
(check (cdr (assoc 'context-window test-meta)) => 1048576)
(check (cdr (assoc "context-window" test-meta)) => 1048576)
(check (cdr (assoc 'max-tokens test-meta)) => 65536)
(check (cdr (assoc "max-tokens" test-meta)) => 65536)
(check (cdr (assoc 'matched? test-meta)) => #t)
(check (cdr (assoc "matched?" test-meta)) => #t)

(check-report)
