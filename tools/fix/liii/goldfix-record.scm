(define-library (liii goldfix-record)
  (export make-fix-token fix-token?
          fix-token-type fix-token-offset fix-token-end
          fix-token-line fix-token-column fix-token-text
          make-fix-line fix-line?
          fix-line-number fix-line-start-offset
          fix-line-first-code-token fix-line-tokens
          make-open-frame open-frame?
          open-frame-offset open-frame-line open-frame-column open-frame-tag-name
          make-pending-close pending-close?
          pending-close-frame pending-close-offset pending-close-end
          pending-close-line pending-close-column
          make-fix-edit fix-edit?
          fix-edit-kind fix-edit-offset fix-edit-start fix-edit-end
          fix-edit-text fix-edit-reason fix-edit-open-offset
          make-repair-report repair-report?
          repair-report-ok? repair-report-edits repair-report-diagnostics)
  (import (liii base)
          (liii error))

  (begin
    (define-record-type fix-token
      (%make-fix-token type offset end line column text)
      fix-token?
      (type fix-token-type)
      (offset fix-token-offset)
      (end fix-token-end)
      (line fix-token-line)
      (column fix-token-column)
      (text fix-token-text))

    (define-record-type fix-line
      (%make-fix-line number start-offset first-code-token tokens)
      fix-line?
      (number fix-line-number)
      (start-offset fix-line-start-offset)
      (first-code-token fix-line-first-code-token)
      (tokens fix-line-tokens))

    (define-record-type open-frame
      (%make-open-frame offset line column tag-name)
      open-frame?
      (offset open-frame-offset)
      (line open-frame-line)
      (column open-frame-column)
      (tag-name open-frame-tag-name))

    (define-record-type pending-close
      (%make-pending-close frame offset end line column)
      pending-close?
      (frame pending-close-frame)
      (offset pending-close-offset)
      (end pending-close-end)
      (line pending-close-line)
      (column pending-close-column))

    (define-record-type fix-edit
      (%make-fix-edit kind offset start end text reason open-offset)
      fix-edit?
      (kind fix-edit-kind)
      (offset fix-edit-offset)
      (start fix-edit-start)
      (end fix-edit-end)
      (text fix-edit-text)
      (reason fix-edit-reason)
      (open-offset fix-edit-open-offset))

    (define-record-type repair-report
      (%make-repair-report ok? edits diagnostics)
      repair-report?
      (ok? repair-report-ok?)
      (edits repair-report-edits)
      (diagnostics repair-report-diagnostics))

    (define (assert-non-negative-integer name value)
      (when (not (integer? value))
        (value-error (string-append name " must be an integer")))
      (when (< value 0)
        (value-error (string-append name " must be non-negative"))))

    (define* (make-fix-token (type 'other)
                             (offset 0)
                             (end 0)
                             (line 1)
                             (column 0)
                             (text ""))
      (when (not (symbol? type))
        (value-error "make-fix-token: type must be a symbol"))
      (assert-non-negative-integer "make-fix-token: offset" offset)
      (assert-non-negative-integer "make-fix-token: end" end)
      (assert-non-negative-integer "make-fix-token: line" line)
      (assert-non-negative-integer "make-fix-token: column" column)
      (when (< end offset)
        (value-error "make-fix-token: end must be >= offset"))
      (when (not (string? text))
        (value-error "make-fix-token: text must be a string"))
      (%make-fix-token type offset end line column text))

    (define* (make-fix-line (number 1)
                            (start-offset 0)
                            (first-code-token #f)
                            (tokens '()))
      (assert-non-negative-integer "make-fix-line: number" number)
      (assert-non-negative-integer "make-fix-line: start-offset" start-offset)
      (when (not (or (eq? first-code-token #f)
                     (fix-token? first-code-token)))
        (value-error "make-fix-line: first-code-token must be a fix-token or #f"))
      (when (not (list? tokens))
        (value-error "make-fix-line: tokens must be a list"))
      (%make-fix-line number start-offset first-code-token tokens))

    (define* (make-open-frame (offset 0)
                              (line 1)
                              (column 0)
                              (tag-name ""))
      (assert-non-negative-integer "make-open-frame: offset" offset)
      (assert-non-negative-integer "make-open-frame: line" line)
      (assert-non-negative-integer "make-open-frame: column" column)
      (when (not (string? tag-name))
        (value-error "make-open-frame: tag-name must be a string"))
      (%make-open-frame offset line column tag-name))

    (define* (make-pending-close (frame (make-open-frame))
                                 (offset 0)
                                 (end 0)
                                 (line 1)
                                 (column 0))
      (when (not (open-frame? frame))
        (value-error "make-pending-close: frame must be an open-frame"))
      (assert-non-negative-integer "make-pending-close: offset" offset)
      (assert-non-negative-integer "make-pending-close: end" end)
      (assert-non-negative-integer "make-pending-close: line" line)
      (assert-non-negative-integer "make-pending-close: column" column)
      (when (< end offset)
        (value-error "make-pending-close: end must be >= offset"))
      (%make-pending-close frame offset end line column))

    (define* (make-fix-edit (kind 'insert)
                            (offset 0)
                            (start 0)
                            (end 0)
                            (text "")
                            (reason "")
                            (open-offset #f))
      (when (not (or (eq? kind 'insert)
                     (eq? kind 'delete)))
        (value-error "make-fix-edit: kind must be insert or delete"))
      (assert-non-negative-integer "make-fix-edit: offset" offset)
      (assert-non-negative-integer "make-fix-edit: start" start)
      (assert-non-negative-integer "make-fix-edit: end" end)
      (when (< end start)
        (value-error "make-fix-edit: end must be >= start"))
      (when (not (string? text))
        (value-error "make-fix-edit: text must be a string"))
      (when (not (string? reason))
        (value-error "make-fix-edit: reason must be a string"))
      (when (not (or (eq? open-offset #f)
                     (and (integer? open-offset)
                          (>= open-offset 0))))
        (value-error "make-fix-edit: open-offset must be #f or a non-negative integer"))
      (%make-fix-edit kind offset start end text reason open-offset))

    (define* (make-repair-report (ok? #f)
                                 (edits '())
                                 (diagnostics '()))
      (when (not (boolean? ok?))
        (value-error "make-repair-report: ok? must be boolean"))
      (when (not (list? edits))
        (value-error "make-repair-report: edits must be a list"))
      (when (not (list? diagnostics))
        (value-error "make-repair-report: diagnostics must be a list"))
      (%make-repair-report ok? edits diagnostics))
  ) ;begin
) ;define-library
