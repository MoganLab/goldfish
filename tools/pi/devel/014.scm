(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi agent)
  (liii pi message)
) ;import

(check-set-mode! 'report-failed)

;; 验收点 a: 会话级超时兜底存在（agent.scm deadline 机制）且有单测覆盖
;; 传入 session-timeout = -1，迫使 run-agent-loop 启动时立刻超时并返回结构化错误消息
(let* ((cfg (list (cons 'provider "google")
              (cons 'api-key "test-stub-key")
              (cons 'model-id "test-model")
              (cons 'model-meta '())
              (cons 'proxy-url #f)
              (cons 'tools '())
              (cons 'settings '())
              (cons 'timeout 10)
              (cons 'max-turns 10)
            ) ;list
       ) ;cfg
       (init-msgs (list '((role . "user") (content . "Hello timeout"))))
       (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10 -1))
       (last-msg (car (reverse res-msgs)))
      ) ;
  (check (pi-assistant-message-stop-reason last-msg) => "error")
  (check-true (string-contains? (pi-assistant-message-error-message last-msg)
                "Session timeout exceeded"
              ) ;string-contains?
  ) ;check-true
) ;let*

;; 验收点 b: 真实验证：含 U+1F680 与小于号的输入（timeout 30 包裹）在 30 秒内正常返回或结构化报错
;; 断言退出码 0 且 stdout 非空，或退出码 1 且 stderr 人可读——两者皆可，唯独不允许超时挂起
(let* ((tmp-out "/tmp/pi-014-smoke-b.txt")
       (ret (run (string-append "sh -c 'bin/gf pi -p --model gemini-2.5-flash \"🚀 <\" > "
                   tmp-out
                   " 2>&1'"
                 ) ;string-append
              :timeout 30
            ) ;run
       ) ;ret
       (content (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check-true (or (= ret 0) (= ret 1)))
  (check-true (not (string-null? (string-trim-both content))))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 验收点 c: bin/gf test 全量过 (测试 tools/pi/tests/liii/pi-agent-test.scm 等关键测试)
(check (run "bin/gf test tools/pi/tests/liii/pi-agent-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/" :timeout 180) => 0)

;; 验收点 d: bin/gf pi --help 可用
(let* ((tmp-help "/tmp/pi-014-help.txt")
       (ret (run (string-append "sh -c 'bin/gf pi --help > " tmp-help "'") :timeout 30)
       ) ;ret
       (help-txt (if (file-exists? tmp-help) (path-read-text tmp-help) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? help-txt "gf pi [options] [--] [@files...] [messages...]")
  ) ;check-true
  (when (file-exists? tmp-help)
    (delete-file tmp-help)
  ) ;when
) ;let*

(check-report)
