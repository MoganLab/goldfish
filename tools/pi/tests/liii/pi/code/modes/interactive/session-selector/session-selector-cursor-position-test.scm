(import (scheme base)
  (liii check)
  (liii tui)
  (liii tui component)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-cursor-position：TUI 内嵌选择器的搜索光标定位
;; 搜索行位于渲染的第 6 行（0 基），行首为 "> " 前缀，故列偏移为 2 + 显示宽度

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path
  ) ;make-pi-session-info
) ;define

(let ((selector (make-session-selector (list (info "a")) '() (lambda (path) #f) (lambda () #f))
      ) ;selector
     ) ;
  ;; 空搜索：光标落在搜索行起始处（列 2，即 "> " 之后）
  (check (session-selector-cursor-position selector) => '(6 2))
  ;; 输入后列偏移随文本宽度增加
  (component-handle-input selector "gold")
  (check (session-selector-cursor-position selector) => '(6 6))
) ;let

(check-report)
