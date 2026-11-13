;; session-selector-filter-sessions
;; 对会话列表执行 UTF-8 容错过滤与分层打分。
;;
;; 语法
;; ----
;; (session-selector-filter-sessions cache sessions query sort-mode)
;;
;; 参数
;; ----
;; cache : hash-table
;; 搜索缓存表。
;;
;; sessions : list
;; 会话摘要列表。
;;
;; query : string
;; 搜索查询串。
;;
;; sort-mode : symbol
;; 排序模式（'recent 或 'relevance）。
;;
;; 返回值
;; ----
;; list
;; 返回匹配的会话列表。

(import (scheme base)
  (liii check)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector-search)
) ;import

(check-set-mode! 'report-failed)

(let* ((s1 (make-pi-session-info "/s1.jsonl" "id1" "/work" "Alpha" "c"
             "2026-10-20T08:00:00.000Z" 1 "hi" "hi"
           ) ;make-pi-session-info
       ) ;s1
       (s2 (make-pi-session-info "/s2.jsonl" "id2" "/work" #f "c"
             "2026-10-21T08:00:00.000Z" 1 "hi"
             (string-append (string #\a #\l (integer->char #x80) #\p #\h #\a)
               " alpha in body"
             ) ;string-append
           ) ;make-pi-session-info
       ) ;s2
       (bad (make-pi-session-info "/bad.jsonl" "bad" "/work" #f "c"
              "2026-10-22T08:00:00.000Z" 1 "hi"
              (lambda () (error 'value-error "broken"))
            ) ;make-pi-session-info
       ) ;bad
       (cache (make-session-search-cache))
      ) ;
  (check (map pi-session-info-path
           (session-selector-filter-sessions cache (list s2 s1 bad) "" 'relevance)
         ) ;map
    =>
    '("/s2.jsonl" "/s1.jsonl" "/bad.jsonl")
  ) ;check
  (check (map pi-session-info-path
           (session-selector-filter-sessions cache (list s2 s1 bad) "alpha" 'relevance)
         ) ;map
    =>
    '("/s1.jsonl" "/s2.jsonl")
  ) ;check
) ;let*

(check-report)
