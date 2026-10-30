(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 用户消息背景、前景、padding 与 spacer
(let* ((card (make-user-card "ls -la")) (w 40) (lines (component-render card w)))
  (check (component? card) => #t)
  (check (pair? lines) => #t)
  (check (length lines) => 4)
  (check (string-contains? (car lines) "48;2;33;59;73") => #t)
  (check-false (string-contains? (car lines) "ls -la"))
  (check (string-contains? (cadr lines) "48;2;33;59;73") => #t)
  (check (string-contains? (cadr lines) "38;2;222;224;225") => #t)
  (check (string-contains? (cadr lines) " ls -la") => #t)
  (check-false (string-contains? (cadr lines) ">"))
  (check (string-contains? (list-ref lines 2) "48;2;33;59;73") => #t)
  (check (list-ref lines 3) => "")
  ;; 内容与宽度不变时复用整个渲染结果，避免历史卡片逐帧重算。
  (check (eq? lines (component-render card w)) => #t)
  (let ((resized (component-render card 20)))
    (check-false (eq? lines resized))
    (check (eq? resized (component-render card 20)) => #t)
  ) ;let
) ;let*

;; 辅助：提取卡片正文行（去掉首尾 padding 空行与尾部 spacer）
(define (%body-lines lines)
  (if (<= (length lines) 3)
    '()
    (cdr (reverse (list-tail (reverse lines) 2)))
  ) ;if
) ;define

;; 超长行折行：18 个 0123456789（180 字符）必须完整显示，不得被截断吞掉
(let* ((long-text (string-join (make-list 18 "0123456789") ""))
       (w 40)
       (lines (component-render (make-user-card long-text) w))
       (content (%body-lines lines))
      ) ;
  ;; 折行后行数必然多于单行（4 行 = 上下 padding + 单行正文 + spacer）
  (check (> (length lines) 4) => #t)
  ;; 每行去掉 ANSI 后显示宽度不超过终端宽度，否则会被 alt-screen 截断
  (check (every (lambda (line) (<= (display-width line) w)) content) => #t)
  ;; 内容完整：正文中数字字符总数必须等于 180，一个都不能少
  (check
    (length
      (filter (lambda (c) (and (char>=? c #\0) (char<=? c #\9)))
        (apply append (map string->list (map strip-ansi content)))
      ) ;filter
    )
    => 180
  )
  ;; 拼接正文（去掉前导缩进与尾部填充空格）可还原完整原文
  (check (string-join (map string-trim-both (map strip-ansi content)) "")
    => long-text
  )
) ;let*

;; 显式换行的多段落文本折行后仍保持段落结构
(let* ((lines (component-render (make-user-card "aaaa\nbbbb") 10))
       (content (%body-lines lines))
      ) ;
  ;; 两段各占一行（4 字符 + 前导空格 = 5 宽，无需折行）
  (check (length content) => 2)
  (check (string-trim-both (strip-ansi (car content))) => "aaaa")
  (check (string-trim-both (strip-ansi (cadr content))) => "bbbb")
) ;let*

(check-report)
