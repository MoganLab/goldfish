(define-library (liii goldfix-repair)
  (export repair-parentheses fix-string parentheses-balanced?)
  (import (liii base)
          (liii goldfix-record)
          (liii goldfix-tokenize)
          (liii goldfix-edit))

  (begin
    (define (push item stack)
      (cons item stack))

    (define (make-open-frame-from-token token)
      (make-open-frame :offset (fix-token-offset token)
                       :line (fix-token-line token)
                       :column (fix-token-column token)
                       :tag-name ""))

    (define (make-pending-close-from-token frame token)
      (make-pending-close :frame frame
                          :offset (fix-token-offset token)
                          :end (fix-token-end token)
                          :line (fix-token-line token)
                          :column (fix-token-column token)))

    (define (make-insert-edit offset reason frame)
      (make-fix-edit :kind 'insert
                     :offset offset
                     :text ")"
                     :reason reason
                     :open-offset (open-frame-offset frame)))

    (define (make-delete-edit token reason open-offset)
      (make-fix-edit :kind 'delete
                     :start (fix-token-offset token)
                     :end (fix-token-end token)
                     :reason reason
                     :open-offset open-offset))

    (define (make-delete-edit-from-pending pending reason)
      (make-fix-edit :kind 'delete
                     :start (pending-close-offset pending)
                     :end (pending-close-end pending)
                     :reason reason
                     :open-offset (open-frame-offset
                                    (pending-close-frame pending))))

    (define (insert-frame-by-column frame frames)
      (cond
        ((null? frames) (list frame))
        ((< (open-frame-column frame)
            (open-frame-column (car frames)))
         (cons frame frames))
        (else
          (cons (car frames)
                (insert-frame-by-column frame (cdr frames))))))

    (define (sort-frames-by-column frames)
      (let loop ((rest frames)
                 (result '()))
        (if (null? rest)
            result
            (loop (cdr rest)
                  (insert-frame-by-column (car rest) result)))))

    (define (line-start-close-token? token line)
      (line-start-close? token line))

    (define (should-pend-close? closed token line)
      (and (not (line-start-close-token? token line))
           (fix-line-first-code-token line)
           (<= (open-frame-column closed)
               (fix-token-column (fix-line-first-code-token line)))))

    (define (parentheses-balanced? source)
      (let ((tokens (tokenize source)))
        (let loop ((rest tokens)
                   (stack '()))
          (cond
            ((null? rest) (null? stack))
            ((eq? (fix-token-type (car rest)) 'open-paren)
             (loop (cdr rest) (push (car rest) stack)))
            ((eq? (fix-token-type (car rest)) 'close-paren)
             (if (null? stack)
                 #f
                 (loop (cdr rest) (cdr stack))))
            (else (loop (cdr rest) stack))))))

    (define (repair-parentheses source)
      (let ((lines (tokenize-lines source))
            (stack '())
            (pending-closes '())
            (edits '())
            (diagnostics '())
            (last-code-end-offset #f))

        (define (add-edit! edit)
          (set! edits (cons edit edits)))

        (define (add-diagnostic! diagnostic)
          (set! diagnostics (cons diagnostic diagnostics)))

        (define (confirm-pending-closes! line-indent)
          (let ((restore '()))
            (let loop ((rest pending-closes))
              (if (not (null? rest))
                  (let ((pending (car rest)))
                    (if (> line-indent
                           (open-frame-column (pending-close-frame pending)))
                        (begin
                          (add-edit!
                            (make-delete-edit-from-pending pending "premature-close"))
                          (set! restore
                                (cons (pending-close-frame pending) restore))))
                    (loop (cdr rest)))))
            (let push-loop ((frames (sort-frames-by-column restore)))
              (if (not (null? frames))
                  (begin
                    (set! stack (push (car frames) stack))
                    (push-loop (cdr frames)))))
            ; Once a following code line has confirmed a pending close, it is final:
            ; either deleted and restored, or kept as a real close.
            (set! pending-closes '())))

        (define (close-by-indent-boundary! first)
          (let ((boundary-indent (fix-token-column first))
                (line-start-close? (eq? (fix-token-type first) 'close-paren)))
            (let loop ()
              (if (and (not (null? stack))
                       (if line-start-close?
                           (> (open-frame-column (car stack)) boundary-indent)
                           (>= (open-frame-column (car stack)) boundary-indent)))
                  (let ((frame (car stack)))
                    (set! stack (cdr stack))
                    (if last-code-end-offset
                        (add-edit!
                          (make-insert-edit last-code-end-offset
                                            "indent-boundary"
                                            frame))
                        (add-diagnostic!
                          (list 'no-insert-position
                                (open-frame-offset frame))))
                    (loop))))))

        (define (process-close-paren! token line)
          (cond
            ((null? stack)
             (add-edit! (make-delete-edit token "extra-close" #f)))
            ((and (line-start-close-token? token line)
                  (not (= (open-frame-column (car stack))
                          (fix-token-column token))))
             (add-edit!
               (make-delete-edit token
                                 "indent-mismatch-close"
                                 (open-frame-offset (car stack)))))
            (else
              (let ((closed (car stack)))
                (set! stack (cdr stack))
                (if (should-pend-close? closed token line)
                    (set! pending-closes
                          (cons (make-pending-close-from-token closed token)
                                pending-closes)))
                (set! last-code-end-offset (fix-token-end token))))))

        (define (process-token! token line)
          (cond
            ((eq? (fix-token-type token) 'open-paren)
             (set! stack
                   (push (make-open-frame-from-token token) stack))
             (set! last-code-end-offset (fix-token-end token)))
            ((eq? (fix-token-type token) 'close-paren)
             (process-close-paren! token line))
            ((code-token? token)
             (set! last-code-end-offset (fix-token-end token)))))

        (define (process-line! line)
          (let ((first (fix-line-first-code-token line)))
            (if first
                (begin
                  (confirm-pending-closes! (fix-token-column first))
                  (close-by-indent-boundary! first)))
            (let loop ((tokens (fix-line-tokens line)))
              (if (not (null? tokens))
                  (begin
                    (process-token! (car tokens) line)
                    (loop (cdr tokens)))))))

        (define (close-remaining-at-eof!)
          ;; EOF 修复要落在最后一个代码 token 后面。如果文件末尾是行注释，
          ;; 直接插到物理 EOF 会让新增的右括号继续成为注释内容。
          (let ((insert-offset (or last-code-end-offset
                                   (string-length source))))
            (set! pending-closes '())
            (let loop ()
              (if (not (null? stack))
                  (let ((frame (car stack)))
                    (set! stack (cdr stack))
                    (add-edit! (make-insert-edit insert-offset "eof" frame))
                    (loop))))))

        (let loop ((rest lines))
          (if (not (null? rest))
              (begin
                (process-line! (car rest))
                (loop (cdr rest)))))

        (close-remaining-at-eof!)

        (let* ((ordered-edits (reverse edits))
               (repaired (apply-edits source ordered-edits))
               (ok? (parentheses-balanced? repaired)))
          (values repaired
                  (make-repair-report :ok? ok?
                                      :edits ordered-edits
                                      :diagnostics (reverse diagnostics))))))

    (define (fix-string source)
      (call-with-values
        (lambda () (repair-parentheses source))
        (lambda (repaired report) repaired)))
  ) ;begin
) ;define-library
