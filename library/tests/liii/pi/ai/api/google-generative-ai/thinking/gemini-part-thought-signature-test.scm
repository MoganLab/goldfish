;; gemini-part-thought-signature
;; 读取 Gemini part 中的思考签名。
;;
;; 语法
;; ----
;; (gemini-part-thought-signature part)
;;
;; 参数
;; ----
;; part : alist
;; Gemini 响应中的单个 part，可含 thoughtSignature 字段。
;;
;; 返回值
;; ----
;; string | #f
;; part 中的思考签名；不存在或非字符串时返回 #f。

(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

;; 字符串键的 thoughtSignature 字段
(check (gemini-part-thought-signature '(("thoughtSignature" . "sig-1")))
  =>
  "sig-1"
) ;check

;; symbol 键的 thoughtSignature 字段
(check (gemini-part-thought-signature '((thoughtSignature . "sig-1")))
  =>
  "sig-1"
) ;check

;; 与其它字段共存
(check (gemini-part-thought-signature '(("text" . "hi")
                                        ("thought" . #t)
                                        ("thoughtSignature" . "sig-2"))
       ) ;gemini-part-thought-signature
  =>
  "sig-2"
) ;check

;; 缺少 thoughtSignature 字段
(check (gemini-part-thought-signature '(("text" . "hi"))) => #f)

;; thoughtSignature 不是字符串
(check (gemini-part-thought-signature '(("thoughtSignature" . 42))) => #f)

;; 非 alist 输入
(check (gemini-part-thought-signature #f) => #f)
(check (gemini-part-thought-signature '()) => #f)

(check-report)
