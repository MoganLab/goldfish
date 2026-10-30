(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 助手消息透明无背景底色
(let* ((assistant-card (make-assistant-card "这是模型的回答文本。"))
       (w 40)
       (lines (component-render assistant-card w))
      ) ;
  (check (component? assistant-card) => #t)
  (check (pair? lines) => #t)
  (check (string-contains? (car lines) "\x1b;[48;2;") => #f)
  (check (string-contains? (car lines) "这是模型的回答文本。") => #t)
  (check (eq? lines (component-render assistant-card w)) => #t)
  (check-false (eq? lines (component-render assistant-card 20)))
) ;let*

;; 辅助：提取卡片正文行（去掉尾部 spacer 空行）
(define (%body-lines lines)
  (if (<= (length lines) 1)
    '()
    (reverse (cdr (reverse lines)))
  ) ;if
) ;define

;; 超长行折行：18 个 0123456789（180 字符）必须完整显示，不得被 alt-screen 截断吞掉
(let* ((long-text (string-join (make-list 18 "0123456789") ""))
       (w 40)
       (lines (component-render (make-assistant-card long-text) w))
       (content (%body-lines lines))
      ) ;
  ;; 折行后行数必然多于单行（单行正文 + spacer）
  (check (> (length lines) 2) => #t)
  ;; 每行显示宽度不超过终端宽度，否则会被 alt-screen 截断
  (check (every (lambda (line) (<= (display-width line) w)) content) => #t)
  ;; 内容完整：正文中数字字符总数必须等于 180，一个都不能少
  (check
    (length
      (filter (lambda (c) (and (char>=? c #\0) (char<=? c #\9)))
        (apply append (map string->list content))
      ) ;filter
    )
    => 180
  )
  ;; 拼接正文可还原完整原文
  (check (string-join (map string-trim-both content) "") => long-text)
) ;let*

;; 中文长行无空格也可逐字折行
(let* ((zh-line (string-join (make-list 30 "金鱼") ""))
       (lines (component-render (make-assistant-card zh-line) 20))
       (content (%body-lines lines))
      ) ;
  ;; 60 个全角字符（宽 120）在 20 列终端必然折行
  (check (> (length lines) 2) => #t)
  ;; 每行显示宽度不超过终端宽度，且拼接正文可还原完整原文
  (check (every (lambda (line) (<= (display-width line) 20)) content) => #t)
  (check (string-join (map string-trim-both content) "") => zh-line)
) ;let*

;; 显式换行的多段落文本折行后仍保持段落结构
(let* ((lines (component-render (make-assistant-card "aaaa\nbbbb") 10))
       (content (%body-lines lines))
      ) ;
  ;; 两段各占一行（4 字符 < 内容宽度 9，无需折行）
  (check (length content) => 2)
  (check (car content) => "aaaa")
  (check (cadr content) => "bbbb")
) ;let*

;; 短文本渲染结果保持原样（不折行、不加前缀、末尾保留 spacer）
(let* ((lines (component-render (make-assistant-card "hi") 40))
      ) ;
  (check lines => '("hi" ""))
) ;let*

;; 零宽及负宽终端不渲染
(let ((card (make-assistant-card "hello")))
  (check (component-render card 0) => '())
  (check (component-render card -1) => '())
) ;let

(check-report)
