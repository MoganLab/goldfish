(import (liii http)
        (liii string)
        (scheme file))

(define download-url "https://jsonplaceholder.typicode.com/posts/1")
(define output-file "/tmp/goldfish-demo-http-file.json")
(define total-bytes 0)

(when (file-exists? output-file)
  (delete-file output-file))

(define r
  (http-get download-url
            :stream #t
            :output-file output-file
            :callback (lambda (chunk)
                        (set! total-bytes (+ total-bytes (string-length chunk)))
                        #t)))

(define preview
  (call-with-input-file output-file
    (lambda (port)
      (read-string 120 port))))

(display "stream-result-undefined?: ")
(write (undefined? r))
(newline)

(display "file: ")
(display output-file)
(newline)

(display "bytes: ")
(write total-bytes)
(newline)

(display "preview:\n")
(display preview)
(newline)
