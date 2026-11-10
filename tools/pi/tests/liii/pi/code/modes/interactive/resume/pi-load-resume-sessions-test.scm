;; pi-load-resume-sessions
;; pi-load-resume-sessions 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-load-resume-sessions ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii path)
  (liii pi code core session-list)
  (liii pi code modes interactive resume)
) ;import

(check-set-mode! 'report-failed)

;; pi-load-resume-sessions：为 --resume 加载候选会话
;; 返回 (values current all-thunk)：current 为当前会话目录的摘要列表，
;; all-thunk 延迟加载全部项目会话；--session-dir 覆盖时 Current/All 同源，
;; 无需默认 agent 目录布局

;; 1. --session-dir 同时用于 Current/All
(let* ((relative "tests/fixtures/compact")
       (fixture-dir (if (file-exists? relative) relative (string-append "tools/pi/" relative))
       ) ;fixture-dir
      ) ;
  (let-values (((current all)
                (pi-load-resume-sessions `((session-dir . ,fixture-dir))
                  '((agent-dir . "/unused-agent"))
                  (path->string (path-cwd))
                ) ;pi-load-resume-sessions
               ) ;
              ) ;
    (check (length current) => 1)
    (check-true (procedure? all))
    (check (length (all)) => 1)
    (check (pi-session-info-id (car current)) => "sess-fixture")
  ) ;let-values
) ;let*

(check-report)
