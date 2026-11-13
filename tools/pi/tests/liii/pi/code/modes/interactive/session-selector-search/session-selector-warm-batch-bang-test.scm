;; session-selector-warm-batch!
;; 按时间片预算预热会话正文的搜索缓存。
;;
;; 语法
;; ----
;; (session-selector-warm-batch! cache sessions [budget-sec])
;;
;; 参数
;; ----
;; cache : hash-table
;; 搜索缓存表。
;;
;; sessions : list
;; 会话摘要列表。
;;
;; budget-sec : real (可选)
;; 单批时间预算（秒），默认 0.025。
;;
;; 返回值
;; ----
;; pair
;; 返回 (warmed-count . pending?) 点对。

(import (scheme base)
  (liii check)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector-search)
) ;import

(check-set-mode! 'report-failed)

(let* ((s1 (make-pi-session-info "/s1.jsonl" "id1" "/work" #f "c"
             "2026-10-20T08:00:00.000Z" 1 "hi" "hi body-only"
           ) ;make-pi-session-info
       ) ;s1
       (cache (make-session-search-cache))
      ) ;
  (check (session-selector-filter-sessions cache (list s1) "body-only" 'recent #t) => '())
  (let ((res (session-selector-warm-batch! cache (list s1) 1.0)))
    (check (car res) => 1)
    (check (cdr res) => #f)
  ) ;let
  (check (map pi-session-info-path
           (session-selector-filter-sessions cache (list s1) "body-only" 'recent #t)
         ) ;map
    =>
    '("/s1.jsonl")
  ) ;check
  (check (session-selector-warm-batch! cache (list s1) 1.0) => '(0 . #f))
) ;let*

(check-report)
