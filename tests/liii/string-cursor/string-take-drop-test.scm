(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-take: 取前nchars个字符
(check (string-take "abcdef" 0) => "")
(check (string-take "abcdef" 2) => "ab")
(check (string-take "abcdef" 6) => "abcdef")
(check (string-take "我是中国人" 2) => "我是")
(check (string-take "🎉🎊🎁" 2) => "🎉🎊")
(check (string-take "a中b文c" 4) => "a中b文")

;; string-drop: 跳过前nchars个字符
(check (string-drop "abcdef" 0) => "abcdef")
(check (string-drop "abcdef" 2) => "cdef")
(check (string-drop "abcdef" 6) => "")
(check (string-drop "我是中国人" 2) => "中国人")
(check (string-drop "🎉🎊🎁" 1) => "🎊🎁")

;; string-take-right: 取后nchars个字符
(check (string-take-right "abcdef" 0) => "")
(check (string-take-right "abcdef" 2) => "ef")
(check (string-take-right "abcdef" 6) => "abcdef")
(check (string-take-right "我是中国人" 2) => "国人")
(check (string-take-right "🎉🎊🎁" 2) => "🎊🎁")

;; string-drop-right: 跳过后nchars个字符
(check (string-drop-right "abcdef" 0) => "abcdef")
(check (string-drop-right "abcdef" 2) => "abcd")
(check (string-drop-right "abcdef" 6) => "")
(check (string-drop-right "我是中国人" 2) => "我是中")
(check (string-drop-right "🎉🎊🎁" 1) => "🎉🎊")

;; 越界检查
(check-catch 'value-error (string-take "abc" 4))
(check-catch 'value-error (string-drop "abc" 4))
(check-catch 'value-error (string-take-right "abc" 4))
(check-catch 'value-error (string-drop-right "abc" 4))

;; string-null?
(check (string-null? "") => #t)
(check (string-null? "a") => #f)
(check (string-null? "中文") => #f)

(check-report)
