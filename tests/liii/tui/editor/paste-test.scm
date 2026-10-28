(import (scheme base)
  (liii check)
  (liii unicode)
  (liii tui editor paste)
) ;import

(check-set-mode! 'report)

;; 1. bracketed paste 序列识别
(check (%bracketed-paste? "\x1b;[200~hello\x1b;[201~") => #t)
(check (%bracketed-paste? "hello") => #f)
(check (%bracketed-paste? "\x1b;[200~hello") => #f)
(check (%bracketed-paste-content "\x1b;[200~hello\x1b;[201~") => "hello")

;; 2. 归一化：CRLF/CR -> LF，Tab -> 4 空格，过滤控制字符
(check (%paste-normalize "a\r\nb\rc\td") => "a\nb\nc    d")
(check (%paste-normalize (string #\a (integer->char 1) #\b)) => "ab")

;; 3. 大粘贴阈值：>10 行 或 >1000 字符
(check (%paste-large? "1234567890") => #f)
(check (%paste-large? (make-string 1000 #\a)) => #f)
(check (%paste-large? (make-string 1001 #\a)) => #t)
(check (%paste-large? (string-append "a" (make-string 9 #\newline) "b")) => #f)
(check (%paste-large? (string-append "a" (make-string 10 #\newline) "b")) => #t)

;; 4. 标记格式：>1000 字符优先展示字符数，否则展示总行数
(let ((reg (make-paste-registry)))
  (check (paste-registry-add! reg (make-string 1965 #\a))
    => "[paste #1 1965 chars]")
  (check (paste-registry-add! reg (string-append "a" (make-string 11 #\newline) "b"))
    => "[paste #2 +12 lines]")
  ;; 多行且超长：字符数优先
  (check (paste-registry-add! reg (string-append (make-string 1200 #\a)
                                   (make-string 20 #\newline)))
    => "[paste #3 1220 chars]")
  (check (paste-registry-count reg) => 3)
) ;let

;; 5. 提交展开
(let* ((reg (make-paste-registry))
       (text (make-string 1965 #\a))
       (marker (paste-registry-add! reg text))
      ) ;
  (check (paste-expand reg marker) => text)
  (check (paste-expand reg (string-append "x" marker "y"))
    => (string-append "x" text "y"))
  ;; 未知标记保持原样
  (check (paste-expand reg "[paste #9 1 chars]") => "[paste #9 1 chars]")
) ;let*

;; 6. 光标前后的标记识别（按 Unicode 码点索引）
(let* ((reg (make-paste-registry))
       (marker (paste-registry-add! reg (make-string 1965 #\a)))
       (line (string-append "你好" marker))
       (end (+ 2 (utf8-string-length marker)))
      ) ;
  (check (paste-marker-at-end reg line end) => (cons marker 2))
  (check (paste-marker-at-start reg line 2) => (cons marker end))
  (check (paste-marker-at-end reg line 2) => #f)
  (check (paste-marker-at-start reg line 0) => #f)
) ;let*

;; 7. 删除注册项后不再展开
(let* ((reg (make-paste-registry))
       (marker (paste-registry-add! reg (make-string 1965 #\a)))
      ) ;
  (check (paste-registry-remove! reg marker) => #t)
  (check (paste-registry-count reg) => 1)
  (check (paste-expand reg marker) => marker)
) ;let*

(check-report)
