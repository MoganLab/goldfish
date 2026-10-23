(import (liii check))
(import (liii goldfmt scan literal))

(check-set-mode! 'report-failed)

(define (contains? pattern text)
  (and (string-position pattern text) #t)
) ;define

;; rewrite-reader-literals
;; 将源码文本中的 reader 字面量（raw string 与字符字面量）改写为
;; (*raw-string* ...) 与 (*char-literal* ...) 形式的普通 datum。
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
;; 1. #"DELIM"content"DELIM" 形式的 raw string 被改写为
;;    (*raw-string* "字面量" "值")
;; 2. #\ 前缀的字符字面量被改写为 (*char-literal* "字面量" 字符)
;; 3. 字符串字面量与注释中的内容不会被误改写

;; 测试：带定界符的 raw string 改写为 (*raw-string* ...) 形式
(let ((result (rewrite-reader-literals "#\"CODE\"(+ 1 2) (+ 3 4)\"CODE\"")))
  (check (contains? "(*raw-string*" result) => #t)
) ;let

;; 测试：空定界符的 raw string 也会被改写
(let ((result (rewrite-reader-literals "#\"\"hello\"\"")))
  (check (contains? "(*raw-string*" result) => #t)
) ;let

;; 测试：字符字面量改写为 (*char-literal* ...) 形式
(let ((result (rewrite-reader-literals "#\\a")))
  (check (contains? "(*char-literal*" result) => #t)
  (check (contains? "#\\a" result) => #t)
) ;let

;; 测试：普通字符串内容不被改写
(let ((result (rewrite-reader-literals "(display \"hello\")")))
  (check result => "(display \"hello\")")
) ;let

;; 测试：行注释中的 #" 不被改写
(let ((result (rewrite-reader-literals ";; #\"not-a-raw-string\"\n42")))
  (check (contains? "(*raw-string*" result) => #f)
) ;let

;; 测试：块注释中的 #" 不被改写
(let ((result (rewrite-reader-literals "#| #\"not-a-raw-string\" |# 42")))
  (check (contains? "(*raw-string*" result) => #f)
) ;let

;; 测试：raw string 内容中包含引号不受影响
(let ((result (rewrite-reader-literals "#\"D\"x \"y\" z\"D\"")))
  (check (contains? "(*raw-string*" result) => #t)
) ;let

;; 测试：混合改写字符字面量与 raw string
(let ((result (rewrite-reader-literals "(list #\\a #\"D\"str\"D\" 1)")))
  (check (contains? "(*char-literal*" result) => #t)
  (check (contains? "(*raw-string*" result) => #t)
) ;let

;; 测试：普通代码原样保留
(let ((result (rewrite-reader-literals "(+ 1 2)")))
  (check result => "(+ 1 2)")
) ;let

(check-report)
