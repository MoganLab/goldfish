(import (liii check)
        (liii goldfmt-tokenize))

(check-set-mode! 'report-failed)

;; 测试 tokenize：空字符串
(let ((tokens (tokenize "")))
  (check (list? tokens) => #t)
  (check (null? tokens) => #t))

;; 测试 tokenize：只有空白字符
(let ((tokens (tokenize "   \n\t\n  ")))
  (check (list? tokens) => #t)
  (check (null? tokens) => #t))

;; 测试 tokenize：单行注释
(let ((tokens (tokenize ";; 这是一个注释")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'comment)
  (check (cdar tokens) => " 这是一个注释"))

;; 测试 tokenize：空注释
(let ((tokens (tokenize ";;")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'comment)
  (check (cdar tokens) => ""))

;; 测试 tokenize：多个注释
(let ((tokens (tokenize ";; 注释1\n;; 注释2\n;; 注释3")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (cdar tokens) => " 注释1")
  (check (cdadr tokens) => " 注释2")
  (check (car (caddr tokens)) => 'comment))

;; 测试 tokenize：代码和注释混合
(let ((tokens (tokenize "(define x 1)\n;; 注释\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'code)
  (check (caadr tokens) => 'comment)
  (check (cdadr tokens) => " 注释")
  (check (caaddr tokens) => 'code))

;; 测试 tokenize：字符串中的分号不应被视为注释
(let ((tokens (tokenize "(define x \";;不是注释\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code))

;; 测试 tokenize：跨行字符串中的分号
(let ((tokens (tokenize "(define x \"第一行\n;;也不是注释\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code))

;; 测试 tokenize：跨行注释被忽略
(let ((tokens (tokenize "#| 跨行\n注释 |#\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code))

;; 测试 tokenize：跨行注释中的 ;; 被忽略
(let ((tokens (tokenize "#| ;; 这不会变成注释 |#\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code))

;; 测试 tokenize：注释前后有空格
(let ((tokens (tokenize "  ;; 前面有空格的注释  ")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => " 前面有空格的注释"))

;; 测试 tokenize：注释内容中保留空格
(let ((tokens (tokenize ";;   前面有多个空格")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => "   前面有多个空格"))

;; 测试 tokenize：注释内容中尾部空格被移除
(let ((tokens (tokenize ";; 尾部有空格   ")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => " 尾部有空格"))

;; 测试 tokenize：只有 ;; 没有内容
(let ((tokens (tokenize ";;")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => ""))

;; 测试 tokenize：;; 后只有一个空格
(let ((tokens (tokenize ";; " )))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => " "))

;; 测试 tokenize：识别单个空行
(let ((tokens (tokenize "(define x 1)\n\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'code)
  (check (caadr tokens) => 'newline)
  (check (cdadr tokens) => 1)
  (check (caaddr tokens) => 'code))

;; 测试 tokenize：识别多个连续空行
(let ((tokens (tokenize "(define x 1)\n\n\n\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caadr tokens) => 'newline)
  (check (cdadr tokens) => 3))

;; 测试 tokenize：注释后的空行
(let ((tokens (tokenize ";; 注释\n\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'comment)
  (check (caadr tokens) => 'newline)
  (check (cdadr tokens) => 1)
  (check (caaddr tokens) => 'code))

;; 测试 tokenize：raw string 中以 ;; 开头的行不是注释
(let ((tokens (tokenize "(define sql #\"\"\n;; not a comment\n\"\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code))

(check-report)
