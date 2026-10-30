(import (scheme base)
  (liii check)
  (liii string)
  (liii tui alt-screen)
  (liii tui terminal))

(check-set-mode! 'report-failed)

;; sync 块顺序与输出去重（对齐上游 tui-alt-screen.ts doRender）：
;; BEGIN_SYNC → 行更新 → 光标定位 → hide → END_SYNC → flush。
;; 光标定位必须纳入 sync 块：行更新会把物理光标推到行首，
;; 若定位序列落在 sync 块外，Windows Terminal 的 IME pre-edit
;; （跟随硬件光标）会在“行首→插入点”之间来回跳，表现为闪烁。

(define (%make-term p)
  (make-standard-terminal :output-port p :detect-size-fn (lambda () (list 20 5)))
) ;define

;; 场景一：带光标参数渲染，光标定位与 hide 序列都在 sync 块内
(let* ((p (open-output-string))
       (vt (%make-term p))
       (screen (make-alt-screen vt))
      ) ;
  (alt-screen-render! screen '("hello") '(3 . 2))
  (let* ((raw (get-output-string p))
         (begin-pos (string-position "\x1b;[?2026h" raw))
         (cur-pos (string-position "\x1b;[3;4H" raw))
         (hide-pos (string-position "\x1b;[?25l" raw))
         (end-pos (string-position "\x1b;[?2026l" raw))
        ) ;let*
    ;; 光标定位与 hide 序列存在（string-position 命中 0 也是存在，布尔化）
    (check-true (and cur-pos (and hide-pos #t)))
    ;; 都位于 sync 块内：begin < cursor < hide < end
    (check-true (and begin-pos (and end-pos #t)))
    (check-true (> cur-pos begin-pos))
    (check-true (> hide-pos cur-pos))
    (check-true (> end-pos hide-pos))
  ) ;let
) ;let*

;; 场景二：帧内容不变、光标移动：只输出 sync 对 + 定位序列，不重绘行内容
(let* ((p (open-output-string))
       (vt (%make-term p))
       (screen (make-alt-screen vt))
      ) ;
  (alt-screen-render! screen '("hello") '(0 . 0))
  (let ((len1 (string-length (get-output-string p))))
    (alt-screen-render! screen '("hello") '(2 . 1))
    (let* ((raw2 (get-output-string p))
           (second-frame (substring raw2 len1 (string-length raw2)))
           (begin-pos (string-position "\x1b;[?2026h" second-frame))
           (cur-pos (string-position "\x1b;[2;3H" second-frame))
           (end-pos (string-position "\x1b;[?2026l" second-frame))
          ) ;let*
      ;; 定位在 sync 块内（布尔化 0 位置）
      (check-true (and begin-pos (and cur-pos (and end-pos #t))))
      (check-true (< begin-pos cur-pos))
      (check-true (< cur-pos end-pos))
      ;; 不重绘未变化的行内容
      (check (string-position "hello" second-frame) => #f)
    ) ;let
  ) ;let
) ;let*

;; 场景三：帧内容与光标都没变：零输出（不重发 sync 对、定位与 hide）
(let* ((p (open-output-string))
       (vt (%make-term p))
       (screen (make-alt-screen vt))
      ) ;
  (alt-screen-render! screen '("hello") '(3 . 2))
  (let ((len1 (string-length (get-output-string p))))
    (alt-screen-render! screen '("hello") '(3 . 2))
    (check (string-length (get-output-string p)) => len1)
  ) ;let
) ;let*

;; 场景四：无光标参数的帧，内容不变时同样零输出
(let* ((p (open-output-string))
       (vt (%make-term p))
       (screen (make-alt-screen vt))
      ) ;
  (alt-screen-render! screen '("hello"))
  (let ((len1 (string-length (get-output-string p))))
    (alt-screen-render! screen '("hello"))
    (check (string-length (get-output-string p)) => len1)
  ) ;let
) ;let*

;; 场景五：terminal-write 后光标位置失效：下一次 set-cursor 必须重发定位
(let* ((p (open-output-string))
       (vt (%make-term p))
      ) ;
  (terminal-set-cursor! vt 3 2)
  (terminal-write vt "hello")
  (terminal-set-cursor! vt 3 2)
  ;; write 推进物理光标，state 过期：再次定位到同一位置也必须输出序列
  (check-true (and (string-position "\x1b;[3;4H" (get-output-string p)) #t))
) ;let*

(check-report)
