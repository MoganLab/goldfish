(import (liii check)
  (liii string)
  (liii tui terminal)
  (liii tui component)
  (liii tui main-screen buffer)
  (liii tui main-screen diff)
  (liii tui main-screen driver)
)

(check-set-mode! 'report-failed)

;; screen-render! 差分渲染与最小写入测试

;; 1. 基础首帧渲染与 virtual-terminal-screen 验证
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      )
  (screen-render! screen '("hello" "world"))
  (check (screen-lines screen) => '("hello" "world"))
  (check (virtual-terminal-screen vt)
    =>
    '("hello     " "world     " "          ")
  )
  ;; 首帧包含 begin-sync, 2 行文本写入, end-sync 共 4 项
  (check (length (virtual-terminal-raw vt)) => 4)
  (check (car (virtual-terminal-raw vt)) => "\x1b[?2026h")
  (check (car (reverse (virtual-terminal-raw vt))) => "\x1b[?2026l")

  ;; 2. 相同内容渲染（零写入，完全无变化）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("hello" "world"))
    (check (length (virtual-terminal-raw vt)) => raw-count-before)
    (check (virtual-terminal-screen vt)
      =>
      '("hello     " "world     " "          ")
    )
  )

  ;; 3. 局部单行修改（仅第 2 行修改，原地覆写，无冗余 clear-line）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("hello" "earth"))
    ;; 帧包裹 begin-sync, 1 次更新写入, end-sync 共新增 3 项
    (check (length (virtual-terminal-raw vt)) => (+ raw-count-before 3))
    (check (virtual-terminal-screen vt)
      =>
      '("hello     " "earth     " "          ")
    )
    (check (list-ref (virtual-terminal-raw vt) (+ raw-count-before 1))
      =>
      "earth"
    )
  )

  ;; 4. 行数缩减（多余行应被清除）
  (screen-render! screen '("only-one"))
  (check (virtual-terminal-screen vt)
    =>
    '("only-one  " "          " "          ")
  )
  (check (screen-lines screen) => '("only-one"))

  ;; 5. 换行文本规范化（单个字符串含 \n 自动拆分）
  (screen-render! screen '("line1\nline2"))
  (check (virtual-terminal-screen vt)
    =>
    '("line1     " "line2     " "          ")
  )
  (check (screen-lines screen) => '("line1" "line2"))

  ;; 6. 光标指定渲染
  (screen-render! screen '("cursor") '(3 0))
  (check (terminal-cursor vt) => '(3 0))
  (check (terminal-cursor-visible? vt) => #t)

  ;; 7. screen-clear! 测试
  (screen-clear! screen)
  (check (screen-lines screen) => '())
  (check (virtual-terminal-screen vt)
    =>
    '("          " "          " "          ")
  )
)

;; 8. Component 组件渲染测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
       (comp (make-text "Goldfish TUI Screen"))
      )
  (screen-render! screen comp)
  (check (pair? (screen-lines screen)) => #t)
  (check (string-starts? (car (virtual-terminal-screen vt)) "Goldfish") => #t)
)

;; 9. ANSI 序列捕获测试（对接 standard-terminal）
(let* ((port (open-output-string))
       (st (make-standard-terminal :output-port port))
       (screen (make-main-screen st))
      )
  (screen-render! screen '("ANSI output" "second line"))
  (let ((ansi-str (get-output-string port)))
    ;; 验证包含光标定位与 DECSET 2026 同步转义序列
    (check (string-contains? ansi-str "\x1b[") => #t)
    (check (string-contains? ansi-str "\x1b;[?2026h") => #t)
    (check (string-contains? ansi-str "\x1b;[?2026l") => #t)
    (check (string-contains? ansi-str "ANSI output") => #t)
    (check (string-contains? ansi-str "second line") => #t)
  )
)

;; 10. 光标定位与同步帧包裹及 terminal-flush 调用测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (trace '())
       (wrapped-term
         (make-terminal-record 'virtual
           (lambda (t) (terminal-size vt))
           (lambda (t text) (terminal-write vt text))
           (lambda (t) (terminal-cursor vt))
           (lambda (t x y)
             (set! trace (cons (list 'set-cursor x y) trace))
             (terminal-set-cursor! vt x y)
           ) ;lambda
           (lambda (t) (terminal-clear vt))
           (lambda (t) (terminal-clear-line vt))
           (lambda (t) (terminal-clear-line-to-end vt))
           (lambda (t)
             (set! trace (cons 'begin-sync trace))
             (terminal-begin-sync! vt)
           ) ;lambda
           (lambda (t)
             (set! trace (cons 'end-sync trace))
             (terminal-end-sync! vt)
           ) ;lambda
           (lambda (t) (terminal-cursor-visible? vt))
           (lambda (t v)
             (set! trace (cons (list 'set-visible v) trace))
             (if v (terminal-show-cursor! vt) (terminal-hide-cursor! vt))
           ) ;lambda
           (lambda (t)
             (set! trace (cons 'flush trace))
             (terminal-flush vt)
           ) ;lambda
           #f
         ) ;make-terminal-record
       ) ;wrapped-term
       (screen (make-main-screen wrapped-term :auto-hide-cursor #f))
      )
  (screen-render! screen '("hello" "world") '(2 0))
  (check (terminal-cursor vt) => '(2 0))
  ;; 验证 trace 调用的正确顺序：
  ;; 1. flush 必须是最后一步
  (check (car trace) => 'flush)
  ;; 2. set-cursor (2 0) 必须在 end-sync 之前
  (let ((rev (reverse trace)))
    (check (car rev) => 'begin-sync)
    ;; 倒数第二个应该是 end-sync
    (check (cadr trace) => 'end-sync)
  )

  ;; 3. 内容未发生变更，仅光标移动时，依然会设置光标并 flush
  (set! trace '())
  (screen-render! screen '("hello" "world") '(3 1))
  (check (terminal-cursor vt) => '(3 1))
  (check (car trace) => 'flush)
  (check (member '(set-cursor 3 1) trace) => '((set-cursor 3 1)))
)

;; 11. 类型错误测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      )
  (check-catch 'type-error (screen-render! screen 12345))
  (check-catch 'type-error (screen-render! screen '("ok" 456)))
)

(check-report)
