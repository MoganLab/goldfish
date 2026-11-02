;; gemini-is-thinking-part?
;; 判断 Gemini part 是否为思考内容。
;;
;; 语法
;; ----
;; (gemini-is-thinking-part? part)
;;
;; 参数
;; ----
;; part : alist
;; Gemini 响应中的单个 part，可含 thought 字段。
;;
;; 返回值
;; ----
;; boolean
;; 是思考内容返回 #t，否则返回 #f。

(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(check-true (gemini-is-thinking-part? '(("text" . "thinking...")
                                        ("thought" . #t)))
) ;check-true
(check-true (gemini-is-thinking-part? '((thought . #t) (text . "thinking..."))))
(check-false (gemini-is-thinking-part? '(("text" . "regular answer"))))
(check-false (gemini-is-thinking-part? '(("text" . "answer") ("thought" . #f))))

(check-report)
