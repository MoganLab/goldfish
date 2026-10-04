(import (scheme base) (scheme file) (scheme read) (scheme write) (scheme eval)
        (only (liii os) os-temp-dir getpid) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
(audit-check 'ports.memory-kinds
  (lambda () (let ((t (open-input-string "a")) (b (open-input-bytevector (bytevector 1))))
               (list (textual-port? t) (boolean? (binary-port? t)) (input-port? t)
                     (binary-port? b) (boolean? (textual-port? b)) (input-port? b)))) '(#t #t #t #t #t #t))
(audit-check 'ports.close
  (lambda () (let ((p (open-output-string))) (close-port p) (close-port p)
               (list (port? p) (output-port? p) (output-port-open? p)))) '(#t #t #f))
(audit-check 'ports.call-with-port
  (lambda () (let ((p (open-output-string)))
               (list (call-with-values (lambda () (call-with-port p (lambda (p) (values 1 2)))) list)
                     (output-port-open? p)))) '((1 2) #f))
(audit-check 'ports.text-peek
  (lambda () (let ((p (open-input-string "λ🙂")))
               (list (peek-char p) (peek-char p) (read-char p) (read-char p) (eof-object? (read-char p)))))
  '(#\λ #\λ #\λ #\🙂 #t))
(audit-check 'ports.read-string
  (lambda () (let ((p (open-input-string "λ🙂x")))
               (list (read-string 2 p) (read-string 8 p) (eof-object? (read-string 1 p))))) '("λ🙂" "x" #t))
(audit-check 'ports.read-line
  (lambda () (let ((p (open-input-string "λ\r🙂\r\nx\ny")))
               (list (read-line p) (read-line p) (read-line p) (read-line p) (eof-object? (read-line p)))))
  '("λ" "🙂" "x" "y" #t))
(audit-check 'ports.ready-eof
  (lambda () (list (char-ready? (open-input-string "")) (u8-ready? (open-input-bytevector (bytevector))))) '(#t #t))
(audit-check 'ports.byte-peek
  (lambda () (let ((p (open-input-bytevector (bytevector 0 255))))
               (list (peek-u8 p) (peek-u8 p) (read-u8 p) (read-u8 p) (eof-object? (read-u8 p))))) '(0 0 0 255 #t))
(audit-check 'ports.read-bytevector
  (lambda () (let ((p (open-input-bytevector (bytevector 0 255 3))))
               (list (read-bytevector 2 p) (read-bytevector 9 p) (eof-object? (read-bytevector 1 p)))))
  (list (bytevector 0 255) (bytevector 3) #t))
(audit-check 'ports.read-bytevector-range
  (lambda () (let ((p (open-input-bytevector (bytevector 1 2))) (b (bytevector 9 9 9 9 9)))
               (let ((n (read-bytevector! b p 1 4))) (list n b (eof-object? (read-bytevector! b p))))))
  (list 2 (bytevector 9 1 2 9 9) #t))
(audit-check 'ports.write-text-range
  (lambda () (let ((p (open-output-string))) (write-string "aλ🙂b" p 1 3) (write-char #\中 p) (get-output-string p))) "λ🙂中")
(audit-check 'ports.write-byte-range
  (lambda () (let ((p (open-output-bytevector))) (write-bytevector (bytevector 1 2 3 4) p 1 3)
               (write-u8 255 p) (get-output-bytevector p))) (bytevector 2 3 255))
(audit-check 'ports.current-output
  (lambda () (let ((p (open-output-string)) (old (current-output-port)))
               (parameterize ((current-output-port p)) (write-string "λ"))
               (list (get-output-string p) (eq? old (current-output-port))))) '("λ" #t))
(audit-check 'ports.flush-default
  (lambda () (let ((p (open-output-string)))
               (parameterize ((current-output-port p)) (write-string "x") (flush-output-port))
               (get-output-string p))) "x")
(audit-check 'ports.write-roundtrip
  (lambda () (let ((p (open-output-string)) (x (list "λ" (string->symbol "two words") (bytevector 0 255))))
               (write x p) (equal? x (read (open-input-string (get-output-string p)))))) #t)
(define (with-audit-file proc)
  (let ((name (string-append (os-temp-dir) "/goldfish-r7rs-port-audit-" (number->string (getpid)))))
    (dynamic-wind (lambda () #f) (lambda () (proc name))
      (lambda () (when (file-exists? name) (delete-file name))))))
(audit-check 'ports.binary-file-kind
  (lambda () (with-audit-file
    (lambda (name)
      (let ((out (open-binary-output-file name)))
        (let ((output-kind (binary-port? out)))
          (close-port out)
          (call-with-port (open-binary-input-file name)
            (lambda (in) (list output-kind (binary-port? in))))))))) '(#t #t))
(audit-check 'ports.file-error-kind
  (lambda () (with-audit-file
    (lambda (name) (guard (e (else (file-error? e))) (open-input-file name) #f)))) #t)
(check-report)
