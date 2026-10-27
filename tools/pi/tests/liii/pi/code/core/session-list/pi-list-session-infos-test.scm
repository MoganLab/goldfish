(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code core session-manager)
  (liii pi code core session-list)
) ;import

(check-set-mode! 'report-failed)

;; pi-list-session-infos：枚举目录下的会话摘要列表，
;; 按最近活动序排列（modified 降序）；可选递归模式供 All scope 使用，
;; 默认只枚举当前目录；目录不存在时返回空列表

(define ROOT
  (path->string (path-join (path-temp-dir) "pi-list-session-infos-test"))
) ;define
(define CURRENT (path->string (path-join ROOT "current")))
(define OTHER (path->string (path-join ROOT "other")))
(define OLD-FILE (path->string (path-join CURRENT "z-old.jsonl")))
(define NEW-FILE (path->string (path-join CURRENT "a-new.jsonl")))
(define OTHER-FILE (path->string (path-join OTHER "other.jsonl")))

(when (file-exists? ROOT)
  (rmtree ROOT)
) ;when
(path-mkdir CURRENT :parents #t :exist-ok #t)
(path-mkdir OTHER :parents #t :exist-ok #t)

;; 文件名顺序与最近消息时间相反，列表必须按活动时间而非文件名排序
(save-session-header! OLD-FILE "old-id" "2026-10-20T08:00:00.000Z" "/work/current")
(save-session-message! OLD-FILE
  "{\"role\":\"user\",\"content\":\"old question\"}"
  "old-user"
  #f
  "2026-10-20T08:00:01.000Z"
) ;save-session-message!
(save-session-header! NEW-FILE "new-id" "2026-10-21T08:00:00.000Z" "/work/current")
(save-session-header! OTHER-FILE "other-id" "2026-10-22T08:00:00.000Z" "/work/other")

;; 1. 默认只枚举当前目录，按 modified 降序
(let ((sessions (pi-list-session-infos CURRENT)))
  (check (map pi-session-info-path sessions)
    => (list NEW-FILE OLD-FILE))
) ;let

;; 2. 递归模式枚举所有子目录（All scope）
(let ((sessions (pi-list-session-infos ROOT #t)))
  (check (map pi-session-info-path sessions)
    => (list OTHER-FILE NEW-FILE OLD-FILE))
) ;let

;; 3. 目录不存在时返回空列表
(check (pi-list-session-infos (path->string (path-join ROOT "missing"))) => '())

(rmtree ROOT)

(check-report)
