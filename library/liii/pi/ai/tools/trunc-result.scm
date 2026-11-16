;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai tools trunc-result)
  (import (scheme base) (liii base))
  (export pi-trunc-result pi-trunc-result? pi-trunc-result-content
    pi-trunc-result-truncated pi-trunc-result-truncated?
    pi-trunc-result-truncated-by pi-trunc-result-total-lines
    pi-trunc-result-total-bytes pi-trunc-result-output-lines
    pi-trunc-result-output-bytes pi-trunc-result-first-line-exceeds-limit?
    pi-trunc-result-first-line-truncated pi-trunc-result-last-line-partial?
    pi-trunc-result-last-line-truncated
  ) ;export
  (begin

    (define*-record-type <pi-trunc-result>
      (pi-trunc-result content
        truncated
        (truncated-by #f)
        (total-lines 0)
        (total-bytes 0)
        (output-lines 0)
        (output-bytes 0)
        (first-line-exceeds-limit? #f)
        (last-line-partial? #f)
      ) ;pi-trunc-result
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
    ) ;define*-record-type

    (define pi-trunc-result-truncated? pi-trunc-result-truncated)

    (define pi-trunc-result-first-line-truncated
      pi-trunc-result-first-line-exceeds-limit?
    ) ;define

    (define pi-trunc-result-last-line-truncated pi-trunc-result-last-line-partial?)

  ) ;begin
) ;define-library
