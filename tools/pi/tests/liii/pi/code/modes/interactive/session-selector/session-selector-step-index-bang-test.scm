;; session-selector-step-index!
;; 在增量索引模式下按时间片步进预热会话正文缓存。
;;
;; 语法
;; ----
;; (session-selector-step-index! comp)
;;
;; 参数
;; ----
;; comp : component
;; 会话选择器组件实例。
;;
;; 返回值
;; ----
;; symbol 或 boolean
;; 全部索引完成或非增量模式返回 #f；有新命中并刷新返回 'updated；仍有待索引条目返回 'more。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

(let* ((s1 (make-pi-session-info "/s1.jsonl" "id1" "/work" "Named" "c"
             "2026-10-20T08:00:00.000Z" 1 "hello" "hello body-token"
           ) ;make-pi-session-info
       ) ;s1
       (sync-sel (make-session-selector (list s1) '() (lambda (p) #f) (lambda () #f)))
       (inc-sel
         (make-session-selector (list s1) '() (lambda (p) #f) (lambda () #f) :incremental #t)
       ) ;inc-sel
      ) ;
  (check (session-selector-step-index! sync-sel) => #f)
  (component-handle-input inc-sel "body-token")
  (check (session-selector-visible-sessions inc-sel) => '())
  (check (session-selector-step-index! inc-sel) => 'updated)
  (check (map pi-session-info-path (session-selector-visible-sessions inc-sel))
    =>
    '("/s1.jsonl")
  ) ;check
  (check (session-selector-step-index! inc-sel) => #f)
) ;let*

(check-report)
