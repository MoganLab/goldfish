;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core tools edit)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii os)
    (liii sort)
    (liii pi code core tools read)
  ) ;import
  (export pi-edit-schema pi-execute-edit)
  (begin

    (define (pi-edit-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "path"
                  (list (cons "type" "string")
                    (cons "description" "Path to the file to edit (relative or absolute)")
                  ) ;list
                ) ;cons
            (cons "edits"
              (list (cons "type" "array")
                (cons "description" "One or more targeted replacements.")
                (cons "items"
                  (list (cons "type" "object")
                    (cons "properties"
                      (list (cons "oldText"
                              (list (cons "type" "string")
                                (cons "description" "Exact text for one targeted replacement.")
                              ) ;list
                            ) ;cons
                        (cons "newText"
                          (list (cons "type" "string")
                            (cons "description" "Replacement text for this targeted edit.")
                          ) ;list
                        ) ;cons
                      ) ;list
                    ) ;cons
                    (cons "required" #("oldText" "newText"))
                  ) ;list
                ) ;cons
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("path" "edits"))
      ) ;list
    ) ;define

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%find-occurrences content pattern)
      (let ((pat-len (string-length pattern)) (content-len (string-length content)))
        (if (zero? pat-len)
          '()
          (let loop
            ((pos 0) (acc '()))
            (cond ((> (+ pos pat-len) content-len) (reverse acc))
                  ((string=? (substring content pos (+ pos pat-len)) pattern)
                   (loop (+ pos pat-len) (cons pos acc))
                  ) ;
                  (else (loop (+ pos 1) acc))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%normalize-edits raw-edits)
      (cond ((vector? raw-edits) (vector->list raw-edits))
            ((list? raw-edits) raw-edits)
            (else '())
      ) ;cond
    ) ;define

    (define (%match-single-edit edit i total-edits content raw-path)
      (let ((old-text (pi-arg-ref edit "oldText" ""))
            (new-text (pi-arg-ref edit "newText" ""))
           ) ;
        (when (string-null? old-text)
          (if (= total-edits 1)
            (error 'error (string-append "oldText must not be empty in " raw-path "."))
            (error 'error
              (string-append "edits["
                (number->string i)
                "].oldText must not be empty in "
                raw-path
                "."
              ) ;string-append
            ) ;error
          ) ;if
        ) ;when
        (let ((matches (%find-occurrences content old-text)))
          (cond
           ((null? matches)
            (if (= total-edits 1)
              (error 'error
                (string-append "Could not find the exact text in "
                  raw-path
                  ". The old text must match exactly including all whitespace and newlines."
                ) ;string-append
              ) ;error
              (error 'error
                (string-append "Could not find edits["
                  (number->string i)
                  "] in "
                  raw-path
                  ". The oldText must match exactly including all whitespace and newlines."
                ) ;string-append
              ) ;error
            ) ;if
           ) ;
           ((> (length matches) 1)
            (if (= total-edits 1)
              (error 'error
                (string-append "Found "
                  (number->string (length matches))
                  " occurrences of the text in "
                  raw-path
                  ". The text must be unique."
                ) ;string-append
              ) ;error
              (error 'error
                (string-append "Found "
                  (number->string (length matches))
                  " occurrences of edits["
                  (number->string i)
                  "] in "
                  raw-path
                  ". Each oldText must be unique."
                ) ;string-append
              ) ;error
            ) ;if
           ) ;
           (else (list i (car matches) (string-length old-text) new-text))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%check-overlaps sorted-edits raw-path)
      (for-each
        (lambda (prev curr)
          (when (> (+ (cadr prev) (caddr prev)) (cadr curr))
            (error 'error
              (string-append "edits["
                (number->string (car prev))
                "] and edits["
                (number->string (car curr))
                "] overlap in "
                raw-path
                ". Merge them into one edit or target disjoint regions."
              ) ;string-append
            ) ;error
          ) ;when
        ) ;lambda
        sorted-edits
        (cdr sorted-edits)
      ) ;for-each
    ) ;define

    (define (%apply-replacements content desc-edits)
      (fold
        (lambda (e res)
          (let* ((start (cadr e))
                 (len (caddr e))
                 (new-txt (cadddr e))
                 (prefix (substring res 0 start))
                 (suffix (substring res (+ start len) (string-length res)))
                ) ;
            (string-append prefix new-txt suffix)
          ) ;let*
        ) ;lambda
        content
        desc-edits
      ) ;fold
    ) ;define

    (define (pi-execute-edit args . opts)
      (let* ((timed-out? (%keyword-value :timed-out? opts #f))
             (cwd (%keyword-value :cwd opts (getcwd)))
             (raw-path (pi-arg-ref args "path"))
             (edits-list (%normalize-edits (pi-arg-ref args "edits" '())))
             (total-edits (length edits-list))
            ) ;
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (unless (string? raw-path)
          (error 'error "path must be a string")
        ) ;unless
        (when (zero? total-edits)
          (error 'error
            "Edit tool input is invalid. edits must contain at least one replacement."
          ) ;error
        ) ;when
        (let* ((abs-path (pi-resolve-path raw-path cwd)))
          (unless (path-exists? (path abs-path))
            (error 'error
              (string-append "Could not edit file: " raw-path ". File not found.")
            ) ;error
          ) ;unless
          (when (path-dir? (path abs-path))
            (error 'error (string-append "Not a file: " raw-path))
          ) ;when
          (let* ((content (path-read-text (path abs-path)))
                 (matched-edits (map (lambda (e i) (%match-single-edit e i total-edits content raw-path))
                                  edits-list
                                  (iota total-edits)
                                ) ;map
                 ) ;matched-edits
                 (sorted
                   (list-sort (lambda (a b) (< (cadr a) (cadr b))) matched-edits)
                 ) ;sorted
                ) ;
            (%check-overlaps sorted raw-path)
            (let* ((desc
                     (list-sort (lambda (a b) (> (cadr a) (cadr b))) matched-edits)
                   ) ;desc
                   (new-content (%apply-replacements content desc))
                  ) ;
              (when (string=? content new-content)
                (if (= total-edits 1)
                  (error 'error
                    (string-append "No changes made to "
                      raw-path
                      ". The replacement produced identical content."
                    ) ;string-append
                  ) ;error
                  (error 'error
                    (string-append "No changes made to "
                      raw-path
                      ". The replacements produced identical content."
                    ) ;string-append
                  ) ;error
                ) ;if
              ) ;when
              (path-write-text (path abs-path) new-content)
              (string-append "Successfully replaced "
                (number->string total-edits)
                " block(s) in "
                raw-path
                "."
              ) ;string-append
            ) ;let*
          ) ;let*
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
