;; rewrite-reader-literals
;; 将源码文本中的 reader 字面量（raw string 与字符字面量）改写为带
;; 一次性 nonce 的内部标记形式（见 (liii goldfmt marker)）。
;;
;; 语法
;; ----
;; (rewrite-reader-literals source)
;;
;; 参数
;; ----
;; source : string?
;; 待改写的源码文本。
;;
;; 返回值
;; ------
;; string?
;; 改写后的源码文本，非字面量部分原样保留。
;;
;; 说明
;; ----
;; 1. #"DELIM"content"DELIM" 形式的 raw string 被改写为 nonce 标记形式
;; 2. #\ 前缀的字符字面量被改写为 nonce 标记形式
;; 3. 字符串字面量与注释中的内容不会被误改写

(import (liii check))
(import (liii goldfmt scan literal) (liii goldfmt marker))

(check-set-mode! 'report-failed)

(define (contains? pattern text)
  (and (string-position pattern text) #t)
) ;define


;; 测试：带定界符的 raw string 改写为 nonce 标记形式
(let ((result (rewrite-reader-literals "#\"CODE\"(+ 1 2) (+ 3 4)\"CODE\"")))
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #t)
) ;let

;; 测试：空定界符的 raw string 也会被改写
(let ((result (rewrite-reader-literals "#\"\"hello\"\"")))
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #t)
) ;let

;; 测试：字符字面量改写为 nonce 标记形式
(let ((result (rewrite-reader-literals "#\\a")))
  (check (contains? (string-append "(" %char-literal-marker-tag-name " ") result) => #t)
  (check (contains? "#\\a" result) => #t)
) ;let

;; 测试：普通字符串内容不被改写
(let ((result (rewrite-reader-literals "(display \"hello\")")))
  (check result => "(display \"hello\")")
) ;let

;; 测试：行注释中的 #" 不被改写
(let ((result (rewrite-reader-literals ";; #\"not-a-raw-string\"\n42")))
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #f)
) ;let

;; 测试：块注释中的 #" 不被改写
(let ((result (rewrite-reader-literals "#| #\"not-a-raw-string\" |# 42")))
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #f)
) ;let

;; 测试：raw string 内容中包含引号不受影响
(let ((result (rewrite-reader-literals "#\"D\"x \"y\" z\"D\"")))
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #t)
) ;let

;; 测试：混合改写字符字面量与 raw string
(let ((result (rewrite-reader-literals "(list #\\a #\"D\"str\"D\" 1)")))
  (check (contains? (string-append "(" %char-literal-marker-tag-name " ") result) => #t)
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #t)
) ;let

;; 测试：普通代码原样保留
(let ((result (rewrite-reader-literals "(+ 1 2)")))
  (check result => "(+ 1 2)")
) ;let

;; 测试：用户写的 (*raw-string* ...) / (*char-literal* ...) 文本不被改写
(let ((result (rewrite-reader-literals "(define e '(*raw-string* \"x\" \"x\"))")))
  (check (contains? "(*raw-string*" result) => #t)
  (check (contains? (string-append "(" %raw-string-marker-tag-name " ") result) => #f)
) ;let

(check-report)
