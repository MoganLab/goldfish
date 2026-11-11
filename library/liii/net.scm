;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii net)
  (export net-listen
    net-listener?
    net-listener-port
    net-listener-addr
    net-listener-close
    net-accept
    net-conn?
    net-conn-peer-addr
    net-conn-read
    net-conn-write
    net-conn-close
    net-dial
    call-with-net-listener
    call-with-net-conn
    http-serve-once
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii string)
  ) ;import
  (begin

    (define (net-listen network address)
      (g_net-listen network address)
    ) ;define

    (define (net-listener? obj)
      (g_net-listener? obj)
    ) ;define

    (define (net-listener-port listener)
      (g_net-listener-port listener)
    ) ;define

    (define (net-listener-addr listener)
      (g_net-listener-addr listener)
    ) ;define

    (define (net-listener-close listener)
      (g_net-listener-close listener)
    ) ;define

    (define (net-accept listener)
      (g_net-accept listener)
    ) ;define

    (define (net-conn? obj)
      (g_net-conn? obj)
    ) ;define

    (define (net-conn-peer-addr conn)
      (g_net-conn-peer-addr conn)
    ) ;define

    (define* (net-conn-read conn (max-bytes 4096))
      (g_net-conn-read conn max-bytes)
    ) ;define*

    (define (net-conn-write conn data)
      (g_net-conn-write conn data)
    ) ;define

    (define (net-conn-close conn)
      (g_net-conn-close conn)
    ) ;define

    (define (net-dial network address)
      (g_net-dial network address)
    ) ;define

    (define (call-with-net-listener network address proc)
      (unless (procedure? proc)
        (type-error "call-with-net-listener: proc must be a procedure" proc)
      ) ;unless
      (let ((ln (net-listen network address)))
        (dynamic-wind
          (lambda () #f)
          (lambda () (proc ln))
          (lambda () (net-listener-close ln))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (call-with-net-conn conn proc)
      (unless (procedure? proc)
        (type-error "call-with-net-conn: proc must be a procedure" proc)
      ) ;unless
      (dynamic-wind
        (lambda () #f)
        (lambda () (proc conn))
        (lambda () (net-conn-close conn))
      ) ;dynamic-wind
    ) ;define

    (define (%extract-query-param req-str param)
      (let* ((lines (string-split req-str "\r\n"))
             (first-line (if (pair? lines) (car lines) ""))
             (parts (string-split first-line " "))
             (target (if (and (pair? parts) (pair? (cdr parts))) (cadr parts) ""))
             (target-prefix (string-append param "="))
             (idx (string-position target-prefix target))
            ) ;
        (if idx
          (let* ((value-start (+ idx (string-length target-prefix)))
                 (rest (substring target value-start (string-length target)))
                 (and-idx (string-position "&" rest))
                ) ;
            (if and-idx (substring rest 0 and-idx) rest)
          ) ;let*
          ""
        ) ;if
      ) ;let*
    ) ;define

    (define* (http-serve-once (callback-path "/callback"))
      (call-with-net-listener 'tcp "127.0.0.1:0"
        (lambda (listener)
          (let* ((port (net-listener-port listener))
                 (redirect-uri (string-append "http://127.0.0.1:" (number->string port) callback-path))
                ) ;
            (display (string-append "Listening on " redirect-uri "\n"))
            (flush-output-port (current-output-port))
            (call-with-net-conn (net-accept listener)
              (lambda (conn)
                (let* ((req-bytes (net-conn-read conn 4096))
                       (req-str (if (bytevector? req-bytes) (utf8->string req-bytes) ""))
                       (body "<html><body><h1>Authorization Successful</h1><p>You may close this tab.</p></body></html>")
                       (resp (string-append "HTTP/1.1 200 OK\r\n"
                               "Content-Type: text/html; charset=utf-8\r\n"
                               "Content-Length: " (number->string (string-length body)) "\r\n"
                               "Connection: close\r\n\r\n"
                               body
                             ) ;string-append
                       ) ;resp
                      ) ;
                  (net-conn-write conn resp)
                  (%extract-query-param req-str "code")
                ) ;let*
              ) ;lambda
            ) ;call-with-net-conn
          ) ;let*
        ) ;lambda
      ) ;call-with-net-listener
    ) ;define*

  ) ;begin
) ;define-library
