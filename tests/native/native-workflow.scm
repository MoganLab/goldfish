(import (liii check)
        (scheme base)
        (scheme file)
        (only (srfi srfi-13) string-prefix?)
        (only (liii string) string-join)
        (only (liii path) path path-join path->string)
        (only (liii os) os-sep))

(check-set-mode! 'report-failed)

;; A small cross-library program should preserve ordinary Scheme state and
;; control flow while using the selected SRFI and liii libraries together.
(define workflow-state 0)
(check (begin
         (set! workflow-state (+ workflow-state 1))
         workflow-state)
  => 1)

(check (guard (condition (else 'caught))
         (raise 'workflow-error))
  => 'caught)

(check (call-with-values
         (lambda () (values workflow-state "ready"))
         list)
  => '(1 "ready"))

(check (let ((saved #f) (first #t) (visited '()))
         (let ((value (call/cc (lambda (k) (set! saved k) 'initial))))
           (when first
             (set! first #f)
             (for-each (lambda (item)
                         (set! visited (cons item visited))
                         (when (= item 2) (saved 'resumed)))
                       '(1 2 3))
             (set! visited (cons 'continued visited)))
           (list value (reverse visited))))
       => '(resumed (1 2)))

(check (string-prefix? "native" (string-join '("native" " workflow") ""))
  => #t)

(check (path->string (path-join (path "tmp") "workflow.txt"))
  => (string-append "tmp" (string (os-sep)) "workflow.txt"))

(check (call-with-port (open-input-string "native workflow")
         (lambda (port) (read-string 6 port)))
  => "native")

;; Dynamic string ports restore on escape and are rebound on continuation entry.
(let ((saved #f)
      (outer-port (current-output-port)))
  (let ((result
         (call/cc
          (lambda (done)
            (let ((text
                   (with-output-to-string
                    (lambda ()
                      (display "inside")
                      (call/cc (lambda (k)
                                 (set! saved k)
                                 (done 'escaped)))
                      (display "-again")))))
              (done text))))))
    (check (eq? (current-output-port) outer-port) => #t)
    (if (eq? result 'escaped)
        (saved #t)
        (check result => "inside-again"))))

(let ((saved #f)
      (outer-port (current-input-port)))
  (let ((result
         (call/cc
          (lambda (done)
            (let ((value
                   (with-input-from-string
                    "abc"
                    (lambda ()
                      (read-char)
                      (call/cc (lambda (k)
                                 (set! saved k)
                                 (done 'escaped)))
                      (read-char)))))
              (done value))))))
    (check (eq? (current-input-port) outer-port) => #t)
    (if (eq? result 'escaped)
        (saved #t)
        (check result => #\b))))

(let ((saved #f))
  (let ((result
         (call/cc
          (lambda (done)
            (let ((text
                   (call-with-output-string
                    (lambda (port)
                      (display "a" port)
                      (call/cc (lambda (k)
                                 (set! saved k)
                                 (done 'escaped)))
                      (display "b" port)))))
              (done text))))))
    (if (eq? result 'escaped)
        (saved #t)
        (check result => "ab"))))

(let ((saved #f))
  (let ((result
         (call/cc
          (lambda (done)
            (let ((value
                   (call-with-input-string
                    "abc"
                    (lambda (port)
                      (read-char port)
                      (call/cc (lambda (k)
                                 (set! saved k)
                                 (done 'escaped)))
                      (read-char port)))))
              (done value))))))
    (if (eq? result 'escaped)
        (saved #t)
        (check result => #\b))))

(let ((outer-port (current-output-port)))
  (guard (condition
          (else
           (check (eq? (current-output-port) outer-port) => #t)))
    (with-output-to-string (lambda () (raise 'port-unwind)))
    (check #f => #t)))

(let ((workflow-file "tests/native/native-workflow-tmp.txt"))
  (dynamic-wind
    (lambda ()
      (when (file-exists? workflow-file)
        (delete-file workflow-file)))
    (lambda ()
      (call-with-output-file workflow-file
        (lambda (port) (display "native workflow" port)))
      (check (call-with-input-file workflow-file
               (lambda (port) (read-string 6 port)))
        => "native")

      (let ((saved #f)
            (outer-port (current-output-port)))
        (let ((result
               (call/cc
                (lambda (done)
                  (with-output-to-file
                   workflow-file
                   (lambda ()
                     (display "a")
                     (call/cc (lambda (k)
                                (set! saved k)
                                (done 'escaped)))
                     (display "b")))
                  (done 'complete)))))
          (check (eq? (current-output-port) outer-port) => #t)
          (if (eq? result 'escaped)
              (saved #t)
              (check (call-with-input-file workflow-file
                       (lambda (port) (read-string 10 port)))
                => "ab"))))

      (with-output-to-file workflow-file (lambda () (display "abc")))
      (let ((saved #f))
        (let ((result
               (call/cc
                (lambda (done)
                  (let ((value
                         (with-input-from-file
                          workflow-file
                          (lambda ()
                            (read-char)
                            (call/cc (lambda (k)
                                       (set! saved k)
                                       (done 'escaped)))
                            (read-char)))))
                    (done value))))))
          (if (eq? result 'escaped)
              (saved #t)
              (check result => #\b))))

      (let ((saved #f))
        (let ((result
               (call/cc
                (lambda (done)
                  (call-with-output-file
                   workflow-file
                   (lambda (port)
                     (display "x" port)
                     (call/cc (lambda (k)
                                (set! saved k)
                                (done 'escaped)))
                     (display "y" port)))
                  (done 'complete)))))
          (if (eq? result 'escaped)
              (saved #t)
              (check (call-with-input-file workflow-file
                       (lambda (port) (read-string 10 port)))
                => "xy"))))

      (with-output-to-file workflow-file (lambda () (display "def")))
      (let ((saved #f))
        (let ((result
               (call/cc
                (lambda (done)
                  (let ((value
                         (call-with-input-file
                          workflow-file
                          (lambda (port)
                            (read-char port)
                            (call/cc (lambda (k)
                                       (set! saved k)
                                       (done 'escaped)))
                            (read-char port)))))
                    (done value))))))
          (if (eq? result 'escaped)
              (saved #t)
              (check result => #\e)))))
    (lambda ()
      (when (file-exists? workflow-file)
        (delete-file workflow-file)))))

;; catch's body and handler both stay on the evaluator's captured stack.
(let ((saved #f) (first #t))
  (let ((value
         (catch 'body-tag
           (lambda ()
             (call/cc (lambda (k) (set! saved k) 'initial-body)))
           (lambda args 'unexpected))))
    (if first
        (begin (set! first #f) (saved 'resumed-body))
        (check value => 'resumed-body))))

(check (catch 'outer-tag
         (lambda ()
           (catch 'inner-tag
             (lambda () (throw 'outer-tag 'payload))
             (lambda args 'wrong-handler)))
         (lambda (tag info) tag))
  => 'outer-tag)

(check (catch 'wrong-type-arg
         (lambda () (car 1))
         (lambda (tag info) tag))
  => 'wrong-type-arg)

(let ((saved #f) (first #t) (handler-resumed #f))
  (let ((value
         (catch 'handler-tag
           (lambda () (throw 'handler-tag))
           (lambda (tag info)
             (call/cc (lambda (k)
                        (set! saved k)
                        'handled))))))
    (if first
        (begin (set! first #f) (saved 'handler-again))
        (if (eq? value 'handler-again)
            (set! handler-resumed #t)
            (check value => 'handled)))
    (check handler-resumed => #t)))

(let ((events '()))
  (check (catch 'wind-tag
           (lambda ()
             (dynamic-wind
               (lambda () (set! events (cons 'before events)))
               (lambda () (throw 'wind-tag))
               (lambda () (set! events (cons 'after events)))))
           (lambda (tag info) 'caught))
    => 'caught)
  (check (reverse events) => '(before after)))

(check-report)
