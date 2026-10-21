(import (liii check)
  (liii error)
  (liii string)
  (liii tui main-screen)
  (liii tui terminal)
  (liii pi event types)
  (liii pi modes tui state)
  (liii pi modes tui message)
  (liii pi modes tui viewport)
  (liii pi modes tui layout)
  (liii pi modes tui editor)
  (liii pi modes tui streaming)
  (liii pi modes tui loop)
) ;import

(check-set-mode! 'report-failed)

;; 1.0 extract-stream-delta: 增量文本提取
;; 1.1 纯字符串直接返回
(check (extract-stream-delta "hello") => "hello")
(check (extract-stream-delta "") => "")

;; 1.2 非法/非字符串非对类型安全返回空串
(check (extract-stream-delta #f) => "")
(check (extract-stream-delta 123) => "")
(check (extract-stream-delta '()) => "")

;; 1.3 符号与字符串 key 的 text_delta / delta
(check (extract-stream-delta '((type . "text_delta") (delta . "chunk1"))) => "chunk1")
(check (extract-stream-delta '(("type" . "text_delta") ("delta" . "chunk2"))) => "chunk2")
(check (extract-stream-delta '((type . "text_delta") (text . "chunk3"))) => "chunk3")
(check (extract-stream-delta '(("type" . "text_delta") ("text" . "chunk4"))) => "chunk4")
(check (extract-stream-delta '((delta . "direct-delta"))) => "direct-delta")
(check (extract-stream-delta '((partialResult . "partial-out"))) => "partial-out")

;; 1.4 嵌套 assistantMessageEvent 提取
(check (extract-stream-delta
         '((type . "message_update")
           (assistantMessageEvent . ((type . "text_delta") (delta . "nested-chunk")))
          ) ;list
       ) ;extract-stream-delta
  => "nested-chunk"
) ;check

(check (extract-stream-delta
         '(("type" . "message_update")
           ("assistantMessageEvent" . (("type" . "text_delta") ("delta" . "str-nested")))
          ) ;list
       ) ;extract-stream-delta
  => "str-nested"
) ;check

;; 1.5 <pi-event-message-update> 记录类型提取
(let ((ev (%make-pi-event-message-update
            '()
            '((type . "text_delta") (delta . "record-delta"))
          ) ;%make-pi-event-message-update
      ) ;ev
     ) ;
  (check (extract-stream-delta ev) => "record-delta")
) ;let

;; 1.6 无 delta/text 的事件返回空串
(check (extract-stream-delta '((type . "turn_end"))) => "")

;; 2.0 append-stream-chunk: 消息列表末尾增量追加
;; 2.1 初始为空时自动创建 Assistant 消息
(let ((res (append-stream-chunk '() "first chunk")))
  (check (length res) => 1)
  (check (message-role (car res)) => 'assistant)
  (check (message-content (car res)) => "first chunk")
) ;let

;; 2.2 末尾为 User 消息时追加新的 Assistant 消息
(let* ((msgs (list (make-pi-tui-message 'user "User prompt")))
       (res (append-stream-chunk msgs "Assistant reply"))
      ) ;
  (check (length res) => 2)
  (check (message-role (cadr res)) => 'assistant)
  (check (message-content (cadr res)) => "Assistant reply")
) ;let

;; 2.3 末尾已是 Assistant 消息时原地追加内容
(let* ((msgs (list (make-pi-tui-message 'user "Prompt")
                   (make-pi-tui-message 'assistant "Hello")
             ) ;list
       ) ;msgs
       (res1 (append-stream-chunk msgs " world"))
       (res2 (append-stream-chunk res1 "!"))
      ) ;
  (check (length res1) => 2)
  (check (message-content (cadr res1)) => "Hello world")
  (check (length res2) => 2)
  (check (message-content (cadr res2)) => "Hello world!")
) ;let

;; 2.4 支持传入事件 alist
(let* ((msgs (list (make-pi-tui-message 'assistant "Count: ")))
       (res (append-stream-chunk msgs '((type . "text_delta") (delta . "123"))))
      ) ;
  (check (message-content (car res)) => "Count: 123")
) ;let

;; 2.5 类型错误保护
(check-catch 'type-error (append-stream-chunk "not-a-list" "chunk"))

;; 3.0 State 流式状态操作（state-append-stream-chunk! 与 state-finish-stream!）
(let ((st (make-pi-tui-state :status 'idle :scroll-offset 5 :dirty? #f)))
  (state-append-message! st (make-pi-tui-message 'user "Hello AI"))
  (check (length (state-messages st)) => 1)

  ;; 首次 chunk：激活 streaming 状态、贴底（offset 0）、置脏
  (state-append-stream-chunk! st "I am ")
  (check (state-status st) => 'streaming)
  (check (state-scroll-offset st) => 0)
  (check (state-dirty? st) => #t)
  (check (length (state-messages st)) => 2)
  (check (message-content (cadr (state-messages st))) => "I am ")

  ;; 清除脏标记后追加第二个 chunk
  (state-clear-dirty! st)
  (check (state-dirty? st) => #f)
  (state-append-stream-chunk! st "ready.")
  (check (state-dirty? st) => #t)
  (check (state-status st) => 'streaming)
  (check (message-content (cadr (state-messages st))) => "I am ready.")

  ;; 流式结束：流转回 idle
  (state-clear-dirty! st)
  (state-finish-stream! st)
  (check (state-status st) => 'idle)
  (check (state-dirty? st) => #t)
) ;let

;; 3.1 纯函数式状态更新（state-with-stream-chunk 与 state-with-stream-finished）
(let* ((st0 (make-pi-tui-state :status 'idle))
       (st1 (state-with-stream-chunk st0 "Pure "))
       (st2 (state-with-stream-chunk st1 "functional"))
       (st3 (state-with-stream-finished st2))
      ) ;
  (check (state-status st1) => 'streaming)
  (check (state-scroll-offset st1) => 0)
  (check (state-dirty? st1) => #t)
  (check (message-content (car (state-messages st2))) => "Pure functional")
  (check (state-status st3) => 'idle)
  (check (state-dirty? st3) => #t)
) ;let

;; 4.0 局部增量排版（compute-committed-lines 与 render-messages-viewport-stream）
(let* ((msg1 (make-pi-tui-message 'user "Hello"))
       (msg2 (make-pi-tui-message 'assistant "Initial text"))
       (committed (compute-committed-lines (list msg1) 40))
       (all-msgs (list msg1 msg2))
       ;; 增量排版结果
       (stream-view (render-messages-viewport-stream all-msgs 40 10 0 committed))
       ;; 全量排版对比
       (full-view (render-messages-viewport all-msgs 40 10 0))
      ) ;
  (check-true (list? committed))
  (check stream-view => full-view)
) ;let

;; 5.0 整体屏幕排版与行数恒定（render-tui-screen-lines）
(let* ((layout (compute-tui-layout 80 24 1))
       (st (make-pi-tui-state :cols 80 :rows 24 :status 'streaming))
       (lines (render-tui-screen-lines st layout))
      ) ;
  ;; 屏幕总行数严格恒等于 24
  (check (length lines) => 24)
  ;; 底部状态栏包含 [ STREAMING ]
  (let ((last-line (list-ref lines 23)))
    (check-true (string-contains last-line "[ STREAMING ]"))
  ) ;let
) ;let

;; 6.0 基于 MainScreen 的行级差分渲染与 DECSET 2026 同步包裹
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ms (make-main-screen vt))
       (st (make-pi-tui-state :cols 80 :rows 24 :status 'idle))
      ) ;
  ;; 1. 初始渲染，输出整屏
  (state-append-message! st (make-pi-tui-message 'user "User prompt"))
  (render-tui-screen! ms st)

  ;; 断言 DECSET 2026 同步包裹标志
  (check-true (not (null? (member "\x1b;[?2026h" (virtual-terminal-raw (screen-terminal ms))))))
  (check-true (not (null? (member "\x1b;[?2026l" (virtual-terminal-raw (screen-terminal ms))))))

  ;; 2. 流式追加第一个 chunk
  (stream-screen-render! ms st "Thinking ")
  (let ((screen-lines-1 (virtual-terminal-screen vt)))
    (check-true (string-contains (list-ref screen-lines-1 1) "Thinking "))
  ) ;let

  ;; 3. 流式追加第二个 chunk：验证行级 diff
  (let* ((buf-before (screen-lines ms))
         (updated-st (state-append-stream-chunk! st "deeply..."))
         (layout (compute-tui-layout 80 24 1))
         (lines-after (render-tui-screen-lines updated-st layout))
         (diff-ops (screen-diff buf-before lines-after))
        ) ;
    ;; 在 24 行中，绝大部分（除助手行与状态栏行）都是 'keep
    (let ((keep-count
            (let count ((ops diff-ops) (acc 0))
              (if (null? ops)
                acc
                (count (cdr ops) (if (eq? (diff-op-type (car ops)) 'keep) (+ acc 1) acc))
              ) ;if
            ) ;count
          ) ;keep-count
         ) ;
      ;; 24 行中至少有 22 行是保持不变 ('keep) 的，无需重传给终端
      (check-true (>= keep-count 22))
    ) ;let
  ) ;let

  ;; 4. 执行渲染更新，断言终端屏幕内容就地覆写
  (render-tui-screen! ms st)
  (let ((screen-lines-2 (virtual-terminal-screen vt)))
    (check-true (string-contains (list-ref screen-lines-2 1) "Thinking deeply..."))
  ) ;let
) ;let

;; 7.0 终端打字机行退格/缩短时的 ANSI 清行（terminal-clear-line-to-end）
(let* ((vt (make-virtual-terminal :width 40 :height 10))
       (ms (make-main-screen vt))
      ) ;
  ;; 初始渲染长文本行
  (screen-render! ms (list "Long line to be erased later" "" "" "" "" "" "" "" "" ""))
  (check-true (string-contains (car (virtual-terminal-screen vt)) "Long line to be erased later"))

  ;; 更新为短文本行，触发行差分 update 并擦除尾部
  (screen-render! ms (list "Short" "" "" "" "" "" "" "" "" ""))
  ;; 验证发送了 ANSI \x1b;[K 清行序列
  (check-true (not (null? (member "\x1b;[K" (virtual-terminal-raw vt)))))
  ;; 验证屏幕第一行截断，尾部无残余字符
  (check (car (virtual-terminal-screen vt)) => "Short                                   ")
) ;let

;; 8.0 主事件循环驱动下的流式渲染（run-tui-loop）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ms (make-main-screen vt))
       (st (make-pi-tui-state :cols 80 :rows 24))
      ) ;
  (state-append-message! st (make-pi-tui-message 'user "Test loop"))
  (let ((final-st
          (run-tui-loop vt
            :state st
            :screen ms
            :return-state #t
            :max-ticks 5
            :on-tick (lambda (s ticks)
                       (when (= ticks 0)
                         (state-append-stream-chunk! s "Streamed in tick")
                       ) ;when
                       (when (= ticks 2)
                         (state-set-status! s 'exit)
                       ) ;when
                     ) ;lambda
          ) ;run-tui-loop
        ) ;final-st
       ) ;
    (check (pi-tui-state? final-st) => #t)
    ;; 验证第一行之后包含流式增量文本
    (let ((scr (virtual-terminal-screen vt)))
      (check-true (string-contains (list-ref scr 1) "Streamed in tick"))
    ) ;let
  ) ;let
) ;let

(check-report)
