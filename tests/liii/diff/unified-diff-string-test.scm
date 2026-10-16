(import (liii check) (liii diff) (srfi srfi-13))

(check-set-mode! 'report-failed)

;; unified-diff-string
;; 接收两个多行文本字符串，比对差异并返回拼接后的标准统一差异补丁字符串。
;;
;; 语法
;; ----
;; (unified-diff-string from-text to-text [fromfile [tofile [n]]])
;;
;; 参数
;; ----
;; from-text : string
;; 修改前完整文本内容。
;; to-text : string
;; 修改后完整文本内容。
;; fromfile : string (可选，默认 "a")
;; 差异头部原文件名标签。
;; tofile : string (可选，默认 "b")
;; 差异头部新文件名标签。
;; n : exact-integer? (可选，默认 3)
;; 上下文保留行数，必须为非负整数。
;;
;; 返回值
;; -----
;; string
;; 拼接后的完整补丁字符串；两份文本完全一致时返回空字符串 ""。
;;
;; 错误处理
;; --------
;; type-error 当 from-text 或 to-text 不是字符串、文件名不是字符串、或 n 不是精确整数时。
;; value-error 当 n 为负数时。

;; 测试项1：基础多行字符串差异比对
(let* ((old-text "foo\nbar\nbaz\n")
       (new-text "foo\nbar2\nbaz\n")
       (patch (unified-diff-string old-text new-text "old.txt" "new.txt"))
      ) ;
  (check-true (string? patch))
  (check-true (string-contains patch "--- old.txt\n"))
  (check-true (string-contains patch "+++ new.txt\n"))
  (check-true (string-contains patch "@@ -1,3 +1,3 @@\n"))
  (check-true (string-contains patch " foo\n"))
  (check-true (string-contains patch "-bar\n"))
  (check-true (string-contains patch "+bar2\n"))
  (check-true (string-contains patch " baz\n"))
) ;let*

;; 测试项2：完全一致时返回空字符串
(check (unified-diff-string "same\ntext\n" "same\ntext\n") => "")
(check (unified-diff-string "" "") => "")
(check (unified-diff-string "line" "line") => "")

;; 测试项3：自定义文件名与上下文 n
(let* ((old "1\n2\n3\n4\n5\n")
       (new "1\n2_mod\n3\n4\n5\n")
       (patch (unified-diff-string old new "src/a.scm" "src/b.scm" 0))
      ) ;
  (check-true (string-contains patch "--- src/a.scm\n"))
  (check-true (string-contains patch "+++ src/b.scm\n"))
  (check-true (string-contains patch "-2\n"))
  (check-true (string-contains patch "+2_mod\n"))
  (check-true (not (string-contains patch " 1\n")))
) ;let*

;; 测试项4：Windows CRLF 换行符支持
(let* ((old "foo\r\nbar\r\n")
       (new "foo\r\nbar2\r\n")
       (patch (unified-diff-string old new "a" "b"))
      ) ;
  (check-true (string-contains patch " foo\n"))
  (check-true (string-contains patch "-bar\n"))
  (check-true (string-contains patch "+bar2\n"))
) ;let*

;; 测试项5：参数校验与异常防御
(check-catch 'type-error (unified-diff-string 123 "text"))
(check-catch 'type-error (unified-diff-string "text" 456))
(check-catch 'type-error (unified-diff-string "a" "b" 123))
(check-catch 'type-error (unified-diff-string "a" "b" "a" 456))
(check-catch 'type-error (unified-diff-string "a" "b" "a" "b" 3.5))
(check-catch 'value-error (unified-diff-string "a" "b" "a" "b" -1))

(check-report)
