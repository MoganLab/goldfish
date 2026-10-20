;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tools types)
  (import (scheme base))
  (export %make-pi-tool-definition pi-tool-definition? pi-tool-definition-name
    pi-tool-definition-description pi-tool-definition-parameters
    pi-tool-definition-execute pi-tool-definition-snippet
    pi-tool-definition-guidelines %make-pi-tool-result pi-tool-result?
    pi-tool-result-content pi-tool-result-is-error %make-pi-trunc-result
    pi-trunc-result? pi-trunc-result-content pi-trunc-result-truncated
    pi-trunc-result-truncated-by pi-trunc-result-total-lines
    pi-trunc-result-total-bytes pi-trunc-result-output-lines
    pi-trunc-result-output-bytes pi-trunc-result-first-line-exceeds-limit?
    pi-trunc-result-last-line-partial?
  ) ;export
  (begin

    (define-record-type <pi-tool-definition>
      (%make-pi-tool-definition name description parameters execute snippet
        guidelines
      ) ;%make-pi-tool-definition
      pi-tool-definition?
      (name pi-tool-definition-name)
      (description pi-tool-definition-description)
      (parameters pi-tool-definition-parameters)
      (execute pi-tool-definition-execute)
      (snippet pi-tool-definition-snippet)
      (guidelines pi-tool-definition-guidelines)
    ) ;define-record-type

    (define-record-type <pi-tool-result>
      (%make-pi-tool-result content is-error)
      pi-tool-result?
      (content pi-tool-result-content)
      (is-error pi-tool-result-is-error)
    ) ;define-record-type

    (define-record-type <pi-trunc-result>
      (%make-pi-trunc-result content truncated truncated-by total-lines
        total-bytes output-lines output-bytes first-line-exceeds-limit?
        last-line-partial?
      ) ;%make-pi-trunc-result
      pi-trunc-result?
      (content pi-trunc-result-content)
      (truncated pi-trunc-result-truncated)
      (truncated-by pi-trunc-result-truncated-by)
      (total-lines pi-trunc-result-total-lines)
      (total-bytes pi-trunc-result-total-bytes)
      (output-lines pi-trunc-result-output-lines)
      (output-bytes pi-trunc-result-output-bytes)
      (first-line-exceeds-limit? pi-trunc-result-first-line-exceeds-limit?)
      (last-line-partial? pi-trunc-result-last-line-partial?)
    ) ;define-record-type

  ) ;begin
) ;define-library
