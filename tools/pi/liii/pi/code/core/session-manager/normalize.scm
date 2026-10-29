;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话消息 JSON 归一化：JSON 解出的数组是 vector，
;; 归一化为 list（键恒为 string，见 save-session-message!）
(define-library (liii pi code core session-manager normalize)
  (import (scheme base) (liii list))
  (export pi-normalize-session-msg)
  (begin

    (define (pi-normalize-session-msg msg)
      (if (pair? msg)
        (map
          (lambda (entry)
            (if (and (equal? (car entry) "content") (vector? (cdr entry)))
              (cons (car entry) (vector->list (cdr entry)))
              entry
            ) ;if
          ) ;lambda
          msg
        ) ;map
        msg
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
