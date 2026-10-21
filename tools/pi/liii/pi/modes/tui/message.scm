;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui message)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii string)
    (liii tui text-width)
  ) ;import
  (export <pi-tui-message> make-pi-tui-message pi-tui-message? message-role
    message-content normalize-pi-tui-message message-role-prefix
    message-role-indent render-message-lines
  ) ;export
  (begin

    (define-record-type <pi-tui-message>
      (%make-pi-tui-message role content)
      pi-tui-message?
      (role message-role)
      (content message-content)
    ) ;define-record-type

    (define <pi-tui-message> '<pi-tui-message>)

    (define (make-pi-tui-message role content)
      (unless (memq role '(user assistant system))
        (type-error "make-pi-tui-message: expected role 'user, 'assistant, or 'system")
      ) ;unless
      (unless (string? content)
        (type-error "make-pi-tui-message: expected string content")
      ) ;unless
      (%make-pi-tui-message role content)
    ) ;define

    (define (normalize-pi-tui-message item)
      (cond ((pi-tui-message? item) item)
            ((pair? item)
             (let ((r (car item)) (c (cdr item)))
               (unless (memq r '(user assistant system))
                 (type-error "normalize-pi-tui-message: expected pair with car 'user, 'assistant, or 'system"
                 ) ;type-error
               ) ;unless
               (unless (string? c)
                 (type-error "normalize-pi-tui-message: expected pair with cdr string content")
               ) ;unless
               (make-pi-tui-message r c)
             ) ;let
            ) ;
            (else (type-error "normalize-pi-tui-message: expected <pi-tui-message> or (role . content) pair"
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (message-role-prefix role)
      (case role
       ((user) "\x1b;[34;1m> \x1b;[0m")
       ((assistant) "\x1b;[32;1m● \x1b;[0m")
       ((system) "\x1b;[90m■ \x1b;[0m")
       (else "> ")
      ) ;case
    ) ;define

    (define (message-role-indent role)
      (case role
       ((user) "  ")
       ((assistant) "  ")
       ((system) "  ")
       (else "  ")
      ) ;case
    ) ;define

    (define (render-message-lines msg width)
      (let* ((m (normalize-pi-tui-message msg))
             (role (message-role m))
             (content (message-content m))
             (prefix (message-role-prefix role))
             (prefix-w (display-width prefix))
             (indent (message-role-indent role))
             (body-width (max 1 (- width prefix-w)))
             (wrapped-body (wrap-text content body-width))
            ) ;
        (if (null? wrapped-body)
          (list prefix)
          (cons (string-append prefix (car wrapped-body))
            (map (lambda (line) (if (string=? line "") "" (string-append indent line)))
              (cdr wrapped-body)
            ) ;map
          ) ;cons
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
