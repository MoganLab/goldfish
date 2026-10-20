(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii date-time)
  (liii date-time current-time)
  (liii pi bash)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [052] 验收脚本：超时基准改用单调时钟
;; 逐条对应 tools/pi/devel/052.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 单文件行数 <= 300 行约束
;;   3) date-time 单调 API 语义与 4 个新单调钟测试
;;   4) pi 封装与墙钟/单调钟共存
;;   5) agent 会话超时语义回归
;;   6) 三路 stream 空闲超时回归
;;   7) 静态门禁：超时路径 grep 无墙钟调用
;;   8) 回拨安全模型断言
;;   9) 全量回归：tools/pi 与 date-time 全绿

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-052-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 120)
  =>
  0
) ;check

(display "=== 验收点 2: 任务 052 相关代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/date-time.scm"
               "goldfish/liii/date-time/current-time.scm" "tools/pi/liii/pi.scm"
               "tools/pi/liii/pi/agent.scm"
               "tools/pi/liii/pi/anthropic/stream.scm"
               "tools/pi/liii/pi/gemini/stream.scm"
               "tools/pi/liii/pi/kimi/stream.scm" "tools/pi/liii/pi/time.scm"
               "tests/liii/date-time/current-time/current-monotonic-millis-test.scm"
               "tests/liii/date-time/current-time/current-monotonic-seconds-test.scm"
               "tools/pi/tests/liii/pi/pi-current-monotonic-millis-test.scm"
               "tools/pi/tests/liii/pi/pi-current-monotonic-seconds-test.scm"
               "tools/pi/devel/052.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (check-true
        (<= (length (string-split (path-read-text f) "\n")) 300)
      ) ;check-true
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: date-time 单调 API 语义与 4 个新单调钟测试 ===\n"
) ;display
;; 进程内直接语义断言
(let ((m-sec (current-monotonic-seconds)) (m-ms (current-monotonic-millis)))
  (check-true (integer? m-sec))
  (check-true (exact? m-sec))
  (check-true (>= m-sec 0))
  (check-true (< m-sec 1000000000))
  (check-true (integer? m-ms))
  (check-true (exact? m-ms))
  (check-true (> m-ms 0))
  (check-true (< m-ms 1000000000000))
  (check-true (>= (current-monotonic-seconds) m-sec))
  (check-true (>= (current-monotonic-millis) m-ms))
) ;let
;; 4 个测试文件运行断言
(check (%run-ok? "t-mono-ms"
         "timeout 60 bin/gf test tests/liii/date-time/current-time/current-monotonic-millis-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-mono-sec"
         "timeout 60 bin/gf test tests/liii/date-time/current-time/current-monotonic-seconds-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-pi-mono-ms"
         "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-current-monotonic-millis-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-pi-mono-sec"
         "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-current-monotonic-seconds-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: pi 封装与墙钟/单调钟共存验证 ===\n")
(let ((pi-sec (pi-current-monotonic-seconds))
      (pi-ms (pi-current-monotonic-millis))
      (wall-sec (pi-current-time-seconds))
      (wall-ms (pi-current-time-millis))
      (iso-str (pi-iso-8601-utc-now))
     ) ;
  (check-true (integer? pi-sec))
  (check-true (exact? pi-sec))
  (check-true (integer? pi-ms))
  (check-true (exact? pi-ms))
  (check-true (> wall-sec 1700000000))
  (check-true (> wall-ms 1700000000000))
  (check-true (string? iso-str))
  (check-true (>= (string-length iso-str) 20))
) ;let
(check (%run-ok? "t-wall-sec"
         "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-current-time-seconds-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-wall-ms"
         "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-current-time-millis-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-iso"
         "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-iso-8601-utc-now-test.scm"
         60
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 5: agent 会话超时语义回归 ===\n")
(check (%run-ok? "t-agent"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi-agent-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 6: 三路 stream 空闲超时测试回归 ===\n")
(check (%run-ok? "t-kimi-st"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/kimi/stream/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-gemini-st"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/gemini/stream-parse/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-anthropic-st"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic/stream-parse/"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 7: 静态门禁（超时路径 grep 无墙钟调用） ===\n"
) ;display
(let* ((grep-cmd (string-append "grep -rn \"pi-current-time-seconds\" "
                   "tools/pi/liii/pi/agent.scm "
                   "tools/pi/liii/pi/gemini/stream.scm "
                   "tools/pi/liii/pi/kimi/stream.scm "
                   "tools/pi/liii/pi/anthropic/stream.scm"
                 ) ;string-append
       ) ;grep-cmd
       (res (%run-capture "grep-wall" grep-cmd 30))
       (rc (car res))
       (out (cdr res))
      ) ;
  ;; grep 无匹配退出码为 1，输出为空
  (check rc => 1)
  (check out => "")
) ;let*

(display "=== 验收点 8: 回拨安全模型断言 ===\n")
(let ((idle-timeout?
        (lambda (now last-act idle) (and idle (> (- now last-act) idle)))
      ) ;idle-timeout?
      (session-timeout? (lambda (now deadline) (and deadline (> now deadline))))
     ) ;
  ;; 墙钟回拨场景：NTP 调整使当前墙钟退回 50 秒
  ;; last-act=1000, now=950 -> now - last-act = -50 <= 10 -> 空闲超时失效
  (check-false (idle-timeout? 950 1000 10))
  ;; 单调钟场景：不受 NTP 影响，始终推进 15 秒
  ;; mono-last=100, mono-now=115 -> now - last-act = 15 > 10 -> 正确超时
  (check-true (idle-timeout? 115 100 10))

  ;; 会话超时回拨：start=1000, timeout=10, deadline=1010
  ;; 15秒后墙钟回退到 950 -> now < deadline -> 会话超时失效
  (check-false (session-timeout? 950 1010))
  ;; 单调钟场景：start=100, timeout=10, deadline=110, mono-now=115
  ;; now > deadline -> 会话超时正确触发
  (check-true (session-timeout? 115 110))
) ;let

(display "=== 验收点 9: 全量回归真绿（tools/pi 273/273，date-time 116/116） ===\n"
) ;display
(let* ((res-pi (%run-capture "all-pi" "timeout 300 bin/gf test tools/pi/tests/liii/" 300)
       ) ;res-pi
       (rc-pi (car res-pi))
       (out-pi (cdr res-pi))
      ) ;
  (check rc-pi => 0)
  (check-true (string-contains? out-pi "Passed: 273"))
  (check-false (string-contains? out-pi "Failed Test Files:"))
) ;let*

(let* ((res-dt (%run-capture "all-dt" "timeout 300 bin/gf test tests/liii/date-time/" 300)
       ) ;res-dt
       (rc-dt (car res-dt))
       (out-dt (cdr res-dt))
      ) ;
  (check rc-dt => 0)
  (check-true (string-contains? out-dt "Passed: 116"))
  (check-false (string-contains? out-dt "Failed Test Files:"))
) ;let*

(check-report)
