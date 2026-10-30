(import (scheme base)
  (liii check)
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
  (check (car first-lines) => "\x1b;[38;2;157;165;169m\x1b;[3m 第一步推理\x1b;[0m")
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
    (check (cadr lines) => "\x1b;[38;2;157;165;169m\x1b;[3m 第二步推理\x1b;[0m")
    (check (caddr lines) => "")
    (check (map strip-ansi lines) => '(" 第一步推理" " 第二步推理" ""))
  ) ;let
) ;let*

;; 零宽及负宽终端不渲染，避免异常尺寸下产生无效行。
(let ((card (make-thinking-card "thinking")))
  (check (component-render card 0) => '())
  (check (component-render card -1) => '())
) ;let

(check-report)
