(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; thinking 卡片默认无背景，以 thinkingText 灰蓝色、斜体和 1 空格缩进逐行显示。
(let* ((card (make-thinking-card "第一步推理"))
       (first-lines (component-render card 40))
      ) ;
  (check-true (component? card))
  (check (length first-lines) => 2)
  (check (car first-lines)
    =>
    "\x1b;[38;2;157;165;169m\x1b;[3m 第一步推理\x1b;[0m"
  ) ;check
  (check (cadr first-lines) => "")
  (check-false (string-contains? (car first-lines) "\x1b;[48;2;"))
  ;; 未变化的卡片沿用渲染缓存。
  (check-true (eq? first-lines (component-render card 40)))
  (check-false (eq? first-lines (component-render card 20)))

  ;; 追加行复用同一组件，并使旧渲染缓存失效。
  (thinking-card-append-line! card "第二步推理")
  (let ((lines (component-render card 40)))
    (check-true (eq? card card))
    (check-false (eq? first-lines lines))
    (check (length lines) => 3)
    (check (car lines) => (car first-lines))
    (check (cadr lines)
      =>
      "\x1b;[38;2;157;165;169m\x1b;[3m 第二步推理\x1b;[0m"
    ) ;check
    (check (caddr lines) => "")
    (check (map strip-ansi lines) => '(" 第一步推理" " 第二步推理" ""))
  ) ;let
) ;let*

;; 零宽及负宽终端不渲染，避免异常尺寸下产生无效行。
(let ((card (make-thinking-card "thinking")))
  (check (component-render card 0) => '())
  (check (component-render card -1) => '())
) ;let

;; 辅助：提取卡片正文行（去掉尾部 spacer 空行）
(define (%body-lines lines)
  (if (<= (length lines) 1)
    '()
    (reverse (cdr (reverse lines)))
  ) ;if
) ;define

;; 超长行折行：18 个 0123456789（180 字符）必须完整显示，不得被 alt-screen 截断吞掉
(let* ((long-line (string-join (make-list 18 "0123456789") ""))
       (card (make-thinking-card long-line))
       (w 40)
       (lines (component-render card w))
       (content (%body-lines lines))
      ) ;
  ;; 折行后行数必然多于单行（2 行 = 单行正文 + spacer）
  (check (> (length lines) 2) => #t)
  ;; 每行去掉 ANSI 后显示宽度不超过终端宽度（前导空格 + 内容），否则会被 alt-screen 截断
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
  ;; 拼接正文（去掉前导缩进）可还原完整原文
  (check (string-join (map string-trim-both (map strip-ansi content)) "")
    => long-line
  )
) ;let*

;; 中文长行无空格也可逐字折行
(let* ((zh-line (string-join (make-list 30 "金鱼") ""))
       (lines (component-render (make-thinking-card zh-line) 20))
      ) ;
  ;; 60 个全角字符（宽 120），20 列终端（前导空格后可容 19，即 9 个全角/行）必然折行
  (check (> (length lines) 2) => #t)
  ;; 每行显示宽度不超过终端宽度，且拼接正文可还原完整原文（s7 按字节拆字符，改用拼接验证）
  (check (every (lambda (line) (<= (display-width line) 20)) (%body-lines lines)) => #t)
  (check (string-join (map string-trim-both (map strip-ansi (%body-lines lines))) "")
    => zh-line
  )
) ;let*

(check-report)
