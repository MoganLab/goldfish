(import (scheme base)
  (liii check)
  (liii path)
  (liii tui)
  (liii pi code modes interactive resume)
) ;import

(check-set-mode! 'report-failed)

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

(let ((vt (make-virtual-terminal :width 100 :height 30)))
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt 'enter)
  (check (pi-run-session-selector (list (info "new") (info "old")) '() :terminal vt)
    => "old")
) ;let

(let ((vt (make-virtual-terminal :width 100 :height 30)))
  (virtual-terminal-push-key! vt 'escape)
  (check (pi-run-session-selector (list (info "new")) '() :terminal vt) => #f)
) ;let

;; --session-dir 同时用于 Current/All，且无需默认 agent 目录布局。
(let* ((relative "tests/fixtures/compact")
       (fixture-dir (if (file-exists? relative) relative
                      (string-append "tools/pi/" relative))))
  (let-values (((current all)
                (pi-load-resume-sessions
                  `((session-dir . ,fixture-dir))
                  '((agent-dir . "/unused-agent"))
                  (path->string (path-cwd)))))
    (check (length current) => 1)
    (check-true (procedure? all))
    (check (length (all)) => 1)
    (check (cdr (assoc 'id (car current))) => "sess-fixture")))

(check-report)
