(import (liii check)
        (liii string)
        (liii tui terminal)
        (liii tui component)
        (liii tui main-screen buffer)
        (liii tui main-screen diff)
        (liii tui main-screen driver))

(check-set-mode! 'report-failed)

;; screen-render! 差分渲染与最小写入测试

;; 1. 基础首帧渲染与 virtual-terminal-screen 验证
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt)))
  (screen-render! screen '("hello" "world"))
  (check (screen-lines screen) => '("hello" "world"))
  (check (virtual-terminal-screen vt)
         => '("hello     "
              "world     "
              "          "))
  ;; 首帧应调用 terminal-write 2 次
  (check (length (virtual-terminal-raw vt)) => 2)

  ;; 2. 相同内容渲染（零写入，完全无变化）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("hello" "world"))
    (check (length (virtual-terminal-raw vt)) => raw-count-before)
    (check (virtual-terminal-screen vt)
           => '("hello     "
                "world     "
                "          ")))

  ;; 3. 局部单行修改（仅第 2 行修改，只写入 1 次，第 1 行不重复写入）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("hello" "earth"))
    (check (length (virtual-terminal-raw vt)) => (+ raw-count-before 1))
    (check (virtual-terminal-screen vt)
           => '("hello     "
                "earth     "
                "          "))
    (check (list-ref (virtual-terminal-raw vt) raw-count-before) => "earth"))

  ;; 4. 行数缩减（多余行应被清除）
  (screen-render! screen '("only-one"))
  (check (virtual-terminal-screen vt)
         => '("only-one  "
              "          "
              "          "))
  (check (screen-lines screen) => '("only-one"))

  ;; 5. 换行文本规范化（单个字符串含 \n 自动拆分）
  (screen-render! screen '("line1\nline2"))
  (check (virtual-terminal-screen vt)
         => '("line1     "
              "line2     "
              "          "))
  (check (screen-lines screen) => '("line1" "line2"))

  ;; 6. 光标指定渲染
  (screen-render! screen '("cursor") '(3 0))
  (check (terminal-cursor vt) => '(3 0))
  (check (terminal-cursor-visible? vt) => #t)

  ;; 7. screen-clear! 测试
  (screen-clear! screen)
  (check (screen-lines screen) => '())
  (check (virtual-terminal-screen vt)
         => '("          "
              "          "
              "          ")))

;; 8. Component 组件渲染测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
       (comp (make-text "Goldfish TUI Screen")))
  (screen-render! screen comp)
  (check (pair? (screen-lines screen)) => #t)
  (check (string-starts? (car (virtual-terminal-screen vt)) "Goldfish") => #t))

;; 9. ANSI 序列捕获测试（对接 standard-terminal）
(let* ((port (open-output-string))
       (st (make-standard-terminal :output-port port))
       (screen (make-main-screen st)))
  (screen-render! screen '("ANSI output" "second line"))
  (let ((ansi-str (get-output-string port)))
    ;; 验证包含光标定位与控制转义序列
    (check (string-contains? ansi-str "\x1b[") => #t)
    (check (string-contains? ansi-str "ANSI output") => #t)
    (check (string-contains? ansi-str "second line") => #t)))

;; 10. 类型错误测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt)))
  (check-catch 'type-error (screen-render! screen 12345))
  (check-catch 'type-error (screen-render! screen '("ok" 456))))

(check-report)
