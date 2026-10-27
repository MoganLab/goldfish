(import (scheme base) (liii check) (liii unicode) (liii tui editor paste))

(check-set-mode! 'report-failed)

;; 1. bracketed paste 序列识别
(check (%bracketed-paste? "\x1b;[200~hello\x1b;[201~") => #t)
(check (%bracketed-paste? "hello") => #f)
(check (%bracketed-paste? "\x1b;[200~hello") => #f)
(check (%bracketed-paste-content "\x1b;[200~hello\x1b;[201~") => "hello")

;; 2. 归一化：CRLF/CR -> LF，Tab -> 4 空格，过滤控制字符
(check (%paste-normalize "a\r\nb\rc\td") => "a\nb\nc    d")
(check (%paste-normalize (string #\a (integer->char 1) #\b)) => "ab")

;; 3. 大粘贴阈值：>10 行或 >1000 字符
(check (%paste-large? "1234567890") => #f)
(check (%paste-large? (make-string 1000 #\a)) => #f)
(check (%paste-large? (make-string 1001 #\a)) => #t)
(check (%paste-large? (string-append "a" (make-string 9 #\newline) "b")) => #f)
(check (%paste-large? (string-append "a" (make-string 11 #\newline) "b")) => #t)

;; 4. 标记格式：单行大文本按字符数，多行按行数
(let ((reg (make-paste-registry)))
  (check (paste-registry-add! reg (make-string 1965 #\a))
    =>
    "[paste #1 1965 chars]"
  ) ;check
  (check (paste-registry-add! reg (string-append "a" (make-string 11 #\newline) "b"))
    =>
    "[paste #2 +12 lines]"
  ) ;check
  (check (paste-registry-count reg) => 2)
) ;let

;; 5. 提交展开
(let* ((reg (make-paste-registry))
       (text (make-string 1965 #\a))
       (marker (paste-registry-add! reg text))
      ) ;
  (check (paste-expand reg marker) => text)
  (check (paste-expand reg (string-append "x" marker "y"))
    =>
    (string-append "x" text "y")
  ) ;check
  ;; 未知 id 保持原样
  (check (paste-expand reg "[paste #9 1 chars]") => "[paste #9 1 chars]")
) ;let*

;; 6. 光标前的标记整体识别
(let* ((reg (make-paste-registry))
       (marker (paste-registry-add! reg (make-string 1965 #\a)))
       (line (string-append "abc" marker))
      ) ;
  (check (paste-marker-at-end reg line (string-length line)) => (cons 1 3))
  (check (paste-marker-at-end reg line (string-length line)) => '(1 . 3))
  (check (paste-marker-at-end reg line 3) => #f)
  (check (paste-marker-at-start reg line 3) => (cons 1 (string-length line)))
  (check (paste-marker-at-start reg line 0) => #f)
) ;let*

;; 6.1 非 ASCII 文本前的标记：索引按 Unicode 码点计算
(let* ((reg (make-paste-registry))
       (big (utf8-make-string 1001 #\中))
       (marker (paste-registry-add! reg big))
       (line (string-append "你好" marker))
       (end (+ 2 (utf8-string-length marker)))
      ) ;
  (check marker => "[paste #1 1001 chars]")
  (check (paste-marker-at-end reg line end) => (cons 1 2))
  (check (paste-marker-at-start reg line 2) => (cons 1 end))
  (check (paste-expand reg line) => (string-append "你好" big))
) ;let*

;; 7. 删除标记后编号重排（编辑器中会先删掉被删标记文本，再重排剩余文本）
(let* ((reg (make-paste-registry))
       (m1 (paste-registry-add! reg (make-string 1965 #\a)))
       (m2 (paste-registry-add! reg (make-string 1965 #\b)))
       (m3 (paste-registry-add! reg (make-string 1965 #\c)))
       (remaining (list (string-append m2 m3)))
       (renumbered (car (paste-renumber-lines remaining 1)))
      ) ;
  (check (paste-registry-remove! reg 1) => #t)
  (check (paste-registry-count reg) => 2)
  (check renumbered => "[paste #1 1965 chars][paste #2 1965 chars]")
  (check (paste-expand reg renumbered)
    =>
    (string-append (make-string 1965 #\b) (make-string 1965 #\c))
  ) ;check
  (check (paste-expand reg m1) => (make-string 1965 #\b))
) ;let*

(check-report)
