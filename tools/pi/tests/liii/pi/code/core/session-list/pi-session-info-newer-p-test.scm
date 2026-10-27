(import (scheme base)
  (liii check)
  (liii pi code core session-list)
) ;import

(check-set-mode! 'report-failed)

;; pi-session-info-newer?：会话摘要的最近活动序比较器
;; modified 降序；modified 相同时按 path 升序打破平局

(define (info path modified)
  (make-pi-session-info path "id" "/work" #f "created" modified 0 "first" "all")
) ;define

;; 1. modified 较新的排在前面
(check-true (pi-session-info-newer? (info "/a.jsonl" "2026-10-21T08:00:00.000Z")
              (info "/b.jsonl" "2026-10-20T08:00:00.000Z")))
(check-false (pi-session-info-newer? (info "/a.jsonl" "2026-10-20T08:00:00.000Z")
               (info "/b.jsonl" "2026-10-21T08:00:00.000Z")))

;; 2. modified 相同时 path 字典序小者在前
(check-true (pi-session-info-newer? (info "/a.jsonl" "2026-10-20T08:00:00.000Z")
              (info "/b.jsonl" "2026-10-20T08:00:00.000Z")))
(check-false (pi-session-info-newer? (info "/b.jsonl" "2026-10-20T08:00:00.000Z")
               (info "/a.jsonl" "2026-10-20T08:00:00.000Z")))

;; 3. 完全相等时不满足严格序
(check-false (pi-session-info-newer? (info "/a.jsonl" "2026-10-20T08:00:00.000Z")
               (info "/a.jsonl" "2026-10-20T08:00:00.000Z")))

(check-report)
