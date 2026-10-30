(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive thinking)
  (liii pi code modes interactive thinking-selector)
) ;import

(check-set-mode! 'report-failed)

(define selected '())
(define saved '())
(define cancel-count 0)
(define (make-selector)
  (make-thinking-selector "medium" pi-thinking-levels "off"
    (lambda (level) (set! selected (cons level selected)))
    (lambda () (set! cancel-count (+ cancel-count 1)))
    (lambda (level) (set! saved (cons level saved)))
  ) ;make-thinking-selector
) ;define

;; 选择器布局、当前级别、默认级别和说明文本对齐上游。
(let* ((selector (make-selector))
       (lines (map strip-ansi (component-render selector 100))))
  (check-true (thinking-selector? selector))
  (check (length lines) => 18)
  (check (list-ref lines 2) => "Thinking Level")
  (check (list-ref lines 4) => "shift+tab cycles thinking levels in-session")
  (check (list-ref lines 6) => ">  ")
  (check-true (string-contains? (list-ref lines 8) "off"))
  (check-true (string-contains? (list-ref lines 8) "No reasoning · default"))
  (check-true (string-contains? (list-ref lines 11) "→ ✓ medium"))
  (check-true (string-contains? (list-ref lines 11) "Moderate reasoning (~8k tokens)"))
  (check-true (string-contains? (list-ref lines 12) "Deep reasoning (~16k tokens)"))
  (check-true (string-contains? (list-ref lines 13) "Extra-high reasoning (~32k tokens)"))
  (check-true (string-contains? (list-ref lines 14) "Maximum reasoning"))
  (check (list-ref lines 16) => "  enter to select · ctrl+s to set as default · esc to cancel")
  (check (select-list-selected-index (thinking-selector-list selector)) => 3)
  (check (thinking-selector-cursor-position selector) => '(6 2))
) ;let*

;; 搜索即时 fuzzy 过滤，Enter 选择当前项；光标随搜索输入移动。
(let ((selector (make-selector)))
  (component-handle-input selector "xh")
  (check (length (select-list-items (thinking-selector-list selector))) => 1)
  (check (thinking-selector-cursor-position selector) => '(6 4))
  (component-handle-input selector 'enter)
  (check selected => '("xhigh"))
) ;let

;; Ctrl+S 保存当前项为默认级别；Escape 和 Ctrl+C 都取消。
(let ((selector (make-selector)))
  (component-handle-input selector 'down)
  (component-handle-input selector 'ctrl-s)
  (check saved => '("high"))
  (component-handle-input selector 'escape)
  (component-handle-input selector 'ctrl-c)
  (check cancel-count => 2)
) ;let

;; 无匹配时显示提示。
(let ((selector (make-selector)))
  (component-handle-input selector "zzz")
  (check-true
    (string-contains?
      (string-join (map strip-ansi (component-render selector 100)) "\n")
      "No matching commands"))
) ;let

(check-report)
