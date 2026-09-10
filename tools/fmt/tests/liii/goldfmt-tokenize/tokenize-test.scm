(import (liii check) (liii goldfmt-tokenize))

(check-set-mode! 'report-failed)

;; 测试 tokenize：空字符串
(let ((tokens (tokenize "")))
  (check (list? tokens) => #t)
  (check (null? tokens) => #t)
) ;let

;; 测试 tokenize：只有空白字符
(let ((tokens (tokenize "   \n\t\n  ")))
  (check (list? tokens) => #t)
  (check (null? tokens) => #t)
) ;let

;; 测试 tokenize：单行注释
(let ((tokens (tokenize ";; 这是一个注释")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'comment)
  (check (cdar tokens) => " 这是一个注释")
) ;let

;; 测试 tokenize：空注释
(let ((tokens (tokenize ";;")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'comment)
  (check (cdar tokens) => "")
) ;let

;; 测试 tokenize：多个注释
(let ((tokens (tokenize ";; 注释1\n;; 注释2\n;; 注释3")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (cdar tokens) => " 注释1")
  (check (cdadr tokens) => " 注释2")
  (check (car (caddr tokens)) => 'comment)
) ;let

;; 测试 tokenize：代码和注释混合
(let ((tokens (tokenize "(define x 1)\n;; 注释\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'code)
  (check (caadr tokens) => 'comment)
  (check (cdadr tokens) => " 注释")
  (check (caaddr tokens) => 'code)
) ;let

;; 测试 tokenize：字符串中的分号不应被视为注释
(let ((tokens (tokenize "(define x \";;不是注释\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
) ;let

;; 测试 tokenize：跨行字符串中的分号
(let ((tokens (tokenize "(define x \"第一行\n;;也不是注释\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
) ;let

;; 测试 tokenize：跨行注释被忽略
(let ((tokens (tokenize "#| 跨行\n注释 |#\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
) ;let

;; 测试 tokenize：跨行注释中的 ;; 被忽略
(let ((tokens (tokenize "#| ;; 这不会变成注释 |#\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
) ;let

;; 测试 tokenize：注释前后有空格
(let ((tokens (tokenize "  ;; 前面有空格的注释  ")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => " 前面有空格的注释")
) ;let

;; 测试 tokenize：注释内容中保留空格
(let ((tokens (tokenize ";;   前面有多个空格")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => "   前面有多个空格")
) ;let

;; 测试 tokenize：注释内容中尾部空格被移除
(let ((tokens (tokenize ";; 尾部有空格   ")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => " 尾部有空格")
) ;let

;; 测试 tokenize：只有 ;; 没有内容
(let ((tokens (tokenize ";;")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => "")
) ;let

;; 测试 tokenize：;; 后只有一个空格
(let ((tokens (tokenize ";; ")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (cdar tokens) => " ")
) ;let

;; 测试 tokenize：识别单个空行
(let ((tokens (tokenize "(define x 1)\n\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'code)
  (check (caadr tokens) => 'newline)
  (check (cdadr tokens) => 1)
  (check (caaddr tokens) => 'code)
) ;let

;; 测试 tokenize：识别多个连续空行
(let ((tokens (tokenize "(define x 1)\n\n\n\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caadr tokens) => 'newline)
  (check (cdadr tokens) => 3)
) ;let

;; 测试 tokenize：注释后的空行
(let ((tokens (tokenize ";; 注释\n\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'comment)
  (check (caadr tokens) => 'newline)
  (check (cdadr tokens) => 1)
  (check (caaddr tokens) => 'code)
) ;let

;; 测试 tokenize：raw string 中以 ;; 开头的行不是注释
(let ((tokens (tokenize "(define sql #\"\"\n;; not a comment\n\"\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
) ;let

;; 测试 tokenize：空 delimiter 的 raw string 结束后，后续行注释仍能被识别
(let ((tokens (tokenize "#\"\"\n  SELECT 1\n  \"\"\n;; 注释\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 3)
  (check (caar tokens) => 'code)
  (check (caadr tokens) => 'comment)
  (check (cdadr tokens) => " 注释")
  (check (caaddr tokens) => 'code)
) ;let

;; 测试 tokenize：行内多个块注释
(let ((tokens (tokenize "(define #| c1 |# x #| c2 |# 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => "(define  x  1)")
) ;let

;; 测试 tokenize：行首与行尾块注释
(let ((tokens (tokenize "#| head |# (define x 1) #| tail |#")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => " (define x 1) ")
) ;let

;; 测试 tokenize：整行只有块注释后接换行
(let ((tokens (tokenize "#| comment |#\n(define x 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => "(define x 1)")
) ;let

;; 测试 tokenize：跨多行的块注释连接前后代码
(let ((tokens (tokenize "(define a #| line1\nline2\nline3 |# 1)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => "(define a  1)")
) ;let

;; 测试 tokenize：长单行代码（无换行大文本）
(let* ((prefix "(list ")
       (suffix ")")
       (long-str (string-append prefix (make-string 20000 #\a) suffix))
       (tokens (tokenize long-str))
      ) ;
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
  (check (string-length (cdar tokens)) => (string-length long-str))
) ;let*

;; 测试 tokenize：带命名 delimiter 的 raw string 内部包含分号和引号
(let ((tokens (tokenize "(define s #\"SQL\"\n;; not comment\n\"str\"\nSQL\")")))
  (check (list? tokens) => #t)
  (check (length tokens) => 1)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => "(define s #\"SQL\"\n;; not comment\n\"str\"\nSQL\")")
) ;let

;; 测试 tokenize：文件末尾无换行的分号注释
(let ((tokens (tokenize "(define x 1) ;; comment without trailing newline")))
  (check (list? tokens) => #t)
  (check (length tokens) => 2)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => "(define x 1) ")
  (check (caadr tokens) => 'comment)
  (check (cdadr tokens) => " comment without trailing newline")
) ;let

;; 测试 tokenize：包含转义双引号和分号的字符串
(let ((tokens (tokenize "(define s \"a\\\"b;;not-comment\")\n(define y 2)")))
  (check (list? tokens) => #t)
  (check (length tokens) => 2)
  (check (caar tokens) => 'code)
  (check (cdar tokens) => "(define s \"a\\\"b;;not-comment\")")
  (check (caadr tokens) => 'code)
  (check (cdadr tokens) => "(define y 2)")
) ;let

(check-report)
