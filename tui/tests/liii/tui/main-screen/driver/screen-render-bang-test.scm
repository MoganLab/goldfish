;; screen-render!
;; 将渲染目标差分渲染到终端表面，并可选地定位硬件光标。
;;
;; 语法
;; ----
;; (screen-render! screen target [cursor])
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; target : list? 或 component?
;; 渲染目标，可以是文本行列表或组件。
;;
;; cursor : pair? 或 list? (可选)
;; 光标位置，形如 (col . row) 或 (col row)。
;;
;; 返回值
;; ----
;; #t
;; 渲染完成返回 #t。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui component)
  (liii tui main-screen buffer)
  (liii tui main-screen diff)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

;; 1. 基础首帧渲染与 virtual-terminal-screen 验证
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("hello" "world"))
  (check (screen-lines screen) => '("hello" "world"))
  (check (virtual-terminal-screen vt)
    =>
    '("hello     " "world     " "          ")
  ) ;check
  ;; 首帧包含 begin-sync, 2 行文本写入, end-sync 共 4 项
  (check (length (virtual-terminal-raw vt)) => 4)
  (check (car (virtual-terminal-raw vt)) => "\x1b;[?2026h")
  (check (car (reverse (virtual-terminal-raw vt))) => "\x1b;[?2026l")

  ;; 2. 相同内容渲染（零写入，完全无变化）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("hello" "world"))
    (check (length (virtual-terminal-raw vt)) => raw-count-before)
    (check (virtual-terminal-screen vt)
      =>
      '("hello     " "world     " "          ")
    ) ;check
  ) ;let

  ;; 3. 局部单行修改（仅第 2 行修改，原地覆写，无冗余 clear-line）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("hello" "earth"))
    ;; 帧包裹 begin-sync, 1 次更新写入, end-sync 共新增 3 项
    (check (length (virtual-terminal-raw vt)) => (+ raw-count-before 3))
    (check (virtual-terminal-screen vt)
      =>
      '("hello     " "earth     " "          ")
    ) ;check
    (check (list-ref (virtual-terminal-raw vt) (+ raw-count-before 1)) => "earth")
  ) ;let

  ;; 4. 行数缩减（多余行应被清除）
  (screen-render! screen '("only-one"))
  (check (virtual-terminal-screen vt)
    =>
    '("only-one  " "          " "          ")
  ) ;check
  (check (screen-lines screen) => '("only-one"))

  ;; 5. 换行文本规范化（单个字符串含 \n 自动拆分）
  (screen-render! screen '("line1\nline2"))
  (check (virtual-terminal-screen vt)
    =>
    '("line1     " "line2     " "          ")
  ) ;check
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
  ) ;check
) ;let*

;; 8. Component 组件渲染测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
       (comp (make-text "Goldfish TUI Screen"))
      ) ;
  (screen-render! screen comp)
  (check (pair? (screen-lines screen)) => #t)
  (check (string-starts? (car (virtual-terminal-screen vt)) "Goldfish") => #t)
) ;let*

;; 9. ANSI 序列捕获测试（对接 standard-terminal）
(let* ((port (open-output-string))
       (st (make-standard-terminal :output-port port))
       (screen (make-main-screen st))
      ) ;
  (screen-render! screen '("ANSI output" "second line"))
  (let ((ansi-str (get-output-string port)))
    ;; 验证包含光标定位与 DECSET 2026 同步转义序列
    (check (string-contains? ansi-str "\x1b;[") => #t)
    (check (string-contains? ansi-str "\x1b;[?2026h") => #t)
    (check (string-contains? ansi-str "\x1b;[?2026l") => #t)
    (check (string-contains? ansi-str "ANSI output") => #t)
    (check (string-contains? ansi-str "second line") => #t)
  ) ;let
) ;let*

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
           (lambda (t) (set! trace (cons 'begin-sync trace)) (terminal-begin-sync! vt))
           (lambda (t) (set! trace (cons 'end-sync trace)) (terminal-end-sync! vt))
           (lambda (t) (terminal-cursor-visible? vt))
           (lambda (t v)
             (set! trace (cons (list 'set-visible v) trace))
             (if v (terminal-show-cursor! vt) (terminal-hide-cursor! vt))
           ) ;lambda
           (lambda (t) (set! trace (cons 'flush trace)) (terminal-flush vt))
           (lambda (t) 0)
           #f
         ) ;make-terminal-record
       ) ;wrapped-term
       (screen (make-main-screen wrapped-term :auto-hide-cursor #f))
      ) ;
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
  ) ;let

  ;; 3. 内容未发生变更，仅光标移动时，依然会设置光标并 flush
  (set! trace '())
  (screen-render! screen '("hello" "world") '(3 1))
  (check (terminal-cursor vt) => '(3 1))
  (check (car trace) => 'flush)
  (check (member '(set-cursor 3 1) trace) => '((set-cursor 3 1)))
) ;let*

;; 11. 防闪烁：等宽行更新为原地覆写，不产生任何 clear 序列
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("old-line-1" "old-line-2"))
  (let ((raw-before (length (virtual-terminal-raw vt))))
    ;; 更新第 2 行，等长文本
    (screen-render! screen '("old-line-1" "new-line-2"))
    (let* ((all-raw (virtual-terminal-raw vt)) (frame-raw (list-tail all-raw raw-before)))
      ;; 帧内包含 begin-sync, new text, end-sync
      (check frame-raw => '("\x1b;[?2026h" "new-line-2" "\x1b;[?2026l"))
      ;; 断言不包含任何 clear 序列
      (check (member "\x1b;[2K\r" frame-raw) => #f)
      (check (member "\x1b;[K" frame-raw) => #f)
    ) ;let*
  ) ;let
) ;let*

;; 12. 防闪烁：缩短行更新后仅在尾部调用 clear-line-to-end
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("long-content"))
  (let ((raw-before (length (virtual-terminal-raw vt))))
    ;; 更新为短文本
    (screen-render! screen '("short"))
    (let* ((all-raw (virtual-terminal-raw vt)) (frame-raw (list-tail all-raw raw-before)))
      ;; 包含 begin-sync, short, clear-to-end, end-sync
      (check frame-raw => '("\x1b;[?2026h" "short" "\x1b;[K" "\x1b;[?2026l"))
      ;; 验证 virtual-terminal-screen 尾部被正确填充为空格
      (check (car (virtual-terminal-screen vt)) => "short               ")
      ;; 断言绝对不含整行清空序列 \x1b[2K\r
      (check (member "\x1b;[2K\r" frame-raw) => #f)
    ) ;let*
  ) ;let
) ;let*

;; 13. 防闪烁：标准终端 ANSI 流中第 2 帧不重复发送隐藏光标序列
(let* ((port2 (open-output-string))
       (st2 (make-standard-terminal :output-port port2))
       (screen2 (make-main-screen st2 :auto-hide-cursor #t))
      ) ;
  (screen-render! screen2 '("frame 1"))
  (let ((len1 (string-length (get-output-string port2))))
    ;; 渲染第 2 帧（光标已是隐藏状态）
    (screen-render! screen2 '("frame 2"))
    (let* ((full-out (get-output-string port2))
           (frame2-out (substring full-out len1 (string-length full-out)))
          ) ;
      ;; 第 2 帧绝不重复发送 \x1b[?25l
      (check (string-contains? frame2-out "\x1b;[?25l") => #f)
      ;; 第 2 帧不包含整行清空 \x1b[2K\r
      (check (string-contains? frame2-out "\x1b;[2K\r") => #f)
    ) ;let*
  ) ;let
) ;let*

;; 14. 逐写入重绘模拟器：任何瞬时中间态都不得出现全空白行
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
       (target-row-snapshots '())
      ) ;
  ;; 初始帧：第 1 行包含内容
  (screen-render! screen '("header" "Heartbeat: ok" "footer"))
  (check (list-ref (virtual-terminal-screen vt) 1) => "Heartbeat: ok       ")

  ;; 自定义包装终端：在底层 write 与 clear 执行时立即捕获屏幕第 1 行
  (let ((wrapped-term
          (make-terminal-record 'virtual
            (lambda (t) (terminal-size vt))
            (lambda (t text)
              (terminal-write vt text)
              (set! target-row-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) target-row-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t) (terminal-cursor vt))
            (lambda (t x y) (terminal-set-cursor! vt x y))
            (lambda (t) (terminal-clear vt))
            (lambda (t)
              (terminal-clear-line vt)
              (set! target-row-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) target-row-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t)
              (terminal-clear-line-to-end vt)
              (set! target-row-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) target-row-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t) (terminal-begin-sync! vt))
            (lambda (t) (terminal-end-sync! vt))
            (lambda (t) (terminal-cursor-visible? vt))
            (lambda (t v) (if v (terminal-show-cursor! vt) (terminal-hide-cursor! vt)))
            (lambda (t) (terminal-flush vt))
            (lambda (t) 0)
            #f
          ) ;make-terminal-record
        ) ;wrapped-term
       ) ;
    (let ((screen-wrapped (make-main-screen wrapped-term)))
      ;; 同步上一帧 buffer 到新屏幕
      (screen-buffer-commit! (screen-buffer screen-wrapped)
        '("header" "Heartbeat: ok" "footer")
      ) ;screen-buffer-commit!
      ;; 触发第 2 帧更新（模拟 Heartbeat 动态变化）
      (screen-render! screen-wrapped '("header" "Heartbeat: pulsing" "footer"))

      ;; 检查所有中间快照：没有任何一次快照中第 1 行是全空白！
      (let ((blank-row (make-string 20 #\space)))
        (for-each (lambda (snap) (check (string=? snap blank-row) => #f))
          target-row-snapshots
        ) ;for-each
      ) ;let
    ) ;let
  ) ;let
) ;let*

;; 15. 内容收缩超过终端高度时触发清屏全量重绘（对齐上游 tui-main-screen）
(let* ((vt (make-virtual-terminal :width 10 :height 10))
       (screen (make-main-screen vt :auto-hide-cursor #f))
      ) ;
  (screen-render! screen '("a" "b" "c" "d" "e"))
  (let ((raw-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("a" "b"))
    ;; screen 缓冲收缩
    (check (screen-lines screen) => '("a" "b"))
    ;; 未触发清屏（局部 diff 即可）
    (check-true (not (member "\x1b;[2J" (virtual-terminal-raw vt))))
    (check-true (> (length (virtual-terminal-raw vt)) raw-before))
  ) ;let
) ;let*

(let* ((vt (make-virtual-terminal :width 10 :height 5))
       (screen (make-main-screen vt :auto-hide-cursor #f))
      ) ;
  (screen-render! screen '("a" "b" "c" "d" "e" "f" "g" "h"))
  ;; 首帧 8 行溢出 5 行高视口，屏幕只保留尾部 5 行
  (screen-render! screen '("a" "b" "c"))
  (check (screen-lines screen) => '("a" "b" "c"))
  ;; 全量重绘后虚拟屏幕恰为收缩后内容，无残留行（局部 diff 清不干净）
  (check (virtual-terminal-screen vt)
    =>
    '("a         " "b         " "c         " "          " "          ")
  ) ;check
) ;let*

;; 16. 类型错误测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (check-catch 'type-error (screen-render! screen 12345))
  (check-catch 'type-error (screen-render! screen '("ok" 456)))
) ;let*

(check-report)
