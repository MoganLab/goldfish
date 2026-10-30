(import (scheme base)
  (liii check)
  (liii string)
  (liii unicode)
  (liii tui editor)
  (liii tui component)
) ;import

(check-set-mode! 'report-failed)

;; editor-render
;; 渲染编辑器组件为终端行列表。
;;
;; 语法
;; ----
;; (component-render ed width)
;;
;; 描述
;; ----
;; - 内容折行宽度为组件宽度减 1：无 padding 时预留最后一列给光标
;;   （对齐上游 pi editor.ts 的 layoutWidth = contentWidth - 1 设计），
;;   内容永不写满整行，避免真实终端写满最后一列后进入 autowrap
;;   待换行状态、随后的清行序列擦掉最后一列字符（Windows 上吞字）；
;; - 上下边框仍使用组件全宽，与终端宽度对齐。

;; 1. 无空格长串按 width-1 硬折行：第一视觉行恰好 width-1 个字符，
;;    最后一列留给光标（回归：150 位数字串最后一位被吞的问题）
(let* ((digits (apply string-append (make-list 15 "0123456789")))
       (ed (make-editor digits :cursor-style 'hardware))
       (lines (component-render ed 10))
      ) ;let*
  ;; 布局宽度 9：150 = 9*16 + 6，共 17 个视觉行
  (check (length lines) => 17)
  (check (car lines) => "012345678")
  (check (cadr lines) => "901234567")
  ;; 最后一个视觉行是余下的 6 位：末尾从 5 数起
  (check (list-ref lines 16) => "456789")
) ;let*

;; 2. 中等宽度：恰好放满一行的内容也提前折行
(let ((ed (make-editor "0123456789" :cursor-style 'hardware)))
  ;; 宽度 10 时布局宽度 9："012345678" 一行，"9" 折到下一行
  (check (component-render ed 10) => '("012345678" "9"))
  ;; 宽度 11 时布局宽度 10：整串一行放完
  (check (component-render ed 11) => '("0123456789"))
) ;let

;; 3. 边框仍使用组件全宽，内容按 width-1 折行、比边框少一个字符
(let* ((ed (make-editor "0123456789" :focused #f :top-border #t :bottom-border #t))
       (lines (component-render ed 10))
      ) ;let*
  (check (length lines) => 4)
  (check (list-ref lines 0) => (utf8-make-string 10 #\─))
  (check (list-ref lines 1) => "012345678")
  (check (list-ref lines 2) => "9")
  (check (list-ref lines 3) => (utf8-make-string 10 #\─))
) ;let*

;; 4. 短内容不受影响：折行宽度足够时不改变渲染结果
(check (component-render (make-editor "hello" :focused #f) 20) => '("hello"))

;; 5. 光标视觉位置与内容折行宽度一致：光标最多到 width-2 列
(let ((ed (make-editor "0123456789" :cursor-style 'hardware)))
  (component-render ed 10)
  ;; 光标在串尾（视觉行 1，第 2 列）
  (check (editor-display-position ed 10) => '(1 1))
) ;let

;; 6. 任务 1196 原始复现：150 位数字串在宽度 80 下折行，
;;    每个视觉行的字符完整（不吞字），且均未写满整宽
(let* ((digits (apply string-append (make-list 15 "0123456789")))
       (ed (make-editor digits :cursor-style 'hardware))
       (lines (component-render ed 80))
       ;; 布局宽度 79：150 = 79 + 71，共 2 个视觉行
       (l0 (car lines))
       (l1 (cadr lines))
      ) ;let*
  (check (length lines) => 2)
  (check l0 => (substring digits 0 79))
  (check l1 => (substring digits 79 150))
  ;; 内容行严格小于组件宽度：最后一列留给光标
  (check (< (string-length l0) 80) => #t)
  (check (< (string-length l1) 80) => #t)
) ;let*

(check-report)
