(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme char)
  (liii check)
  (liii path)
  (liii string)
  (liii list)
  (liii subprocess)
  (liii date-time)
  (liii time)
  (liii pi bash)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [039] 验收脚本：时间函数统一迁移至 (liii date-time)
;; 逐条对应 tools/pi/devel/039.md 验收点：
;;   1) 除 sleep 外无 (liii time) 时间 API 残留（grep 静态门禁）
;;   2) 时间源统一为 (liii date-time)，整秒统一走 pi-current-time-seconds
;;   3) 行为不变（ISO 8601）：YYYY-MM-DDTHH:MM:SS.mmmZ，固定三位毫秒
;;   4) 行为不变（毫秒）：pi-current-time-millis 精确整数、量级正确
;;   5) 整秒时间戳（流式空闲超时口径）与旧实现等价
;;   6) TDD 覆盖：时间函数测试 + 消费方回归 + 全量回归
;;   7) 门禁：build / fmt --check / check / doc
;; 外部命令一律经 (liii subprocess) run 执行并必带 :timeout。
;;
;; 辅助定义与「迁移前旧实现」参考函数见同目录 039-support.scm

(load "tools/pi/devel/039-support.scm")

;; 六个仍需 (liii time) 的文件：该 import 只为 sleep 保留

(define %sleep-files
  '("tools/pi/liii/pi/gemini/request.scm" "tools/pi/liii/pi/gemini/stream.scm"
    "tools/pi/liii/pi/kimi/request.scm" "tools/pi/liii/pi/kimi/stream.scm"
    "tools/pi/liii/pi/anthropic/request.scm"
    "tools/pi/liii/pi/anthropic/stream.scm")
) ;define

;; 统一走 pi-current-time-seconds 的整秒消费方（agent + 三路 SSE）

(define %seconds-consumers
  '("tools/pi/liii/pi/agent.scm" "tools/pi/liii/pi/gemini/stream.scm"
    "tools/pi/liii/pi/kimi/stream.scm" "tools/pi/liii/pi/anthropic/stream.scm")
) ;define

(define (%check-iso-8601 now)
  (check (string-length now) => 24)
  (check (substring now 4 5) => "-")
  (check (substring now 7 8) => "-")
  (check (substring now 10 11) => "T")
  (check (substring now 13 14) => ":")
  (check (substring now 16 17) => ":")
  (check (substring now 19 20) => ".")
  (check (substring now 23 24) => "Z")
  ;; 除分隔符外每个字符都是数字
  (let loop
    ((i 0) (bad 0))
    (cond ((>= i 24) (check bad => 0))
          ((memv i '(4 7 10 13 16 19 23)) (loop (+ i 1) bad))
          ((char-numeric? (string-ref now i)) (loop (+ i 1) bad))
          (else (loop (+ i 1) (+ bad 1)))
    ) ;cond
  ) ;let
) ;define

;; ISO 8601 字符串 -> epoch 毫秒（精确有理数运算，无浮点误差）

(define (%iso->millis s)
  (floor (* 1000 (timestamp->utc-posix-time (iso-8601->timestamp s))))
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (display "=== 验收点 7: 门禁检查（构建 / fmt / check / doc） ===\n")
    (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
    (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
    (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
    (check (%ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 150) => 0)

    (display "=== 验收点 1: 除 sleep 外无 (liii time) 时间 API 残留 ===\n"
    ) ;display
    ;; 词边界匹配，排除 pi-current-time-millis 等 pi 自有封装名
    (check (cdr (%capture "grep-api"
                  (string-append "grep -rnE \"(^|[^-[:alnum:]])(current-time|time-second"
                    "|time-nanosecond|current-second|time-utc->date)\""
                    " tools/pi/liii || true"
                  ) ;string-append
                  60
                ) ;%capture
           ) ;cdr
      =>
      ""
    ) ;check
    (check (cdr (%capture "grep-scheme-time"
                  "grep -rn \"(scheme time)\" tools/pi/liii || true"
                  60
                ) ;%capture
           ) ;cdr
      =>
      ""
    ) ;check
    (let ((lines (%cmd-lines "grep-liii-time"
                   "grep -rn \"(liii time)\" tools/pi/liii || true"
                   60
                 ) ;%cmd-lines
          ) ;lines
         ) ;
      (check (length lines) => 6)
      (for-each
        (lambda (f)
          (check-true (>= (%grep-count "sleep" f) 1))
          (check-true (%has-substr? lines f))
        ) ;lambda
        %sleep-files
      ) ;for-each
    ) ;let

    (display "=== 验收点 2: 时间源统一为 (liii date-time) ===\n")
    (check-true (%has-substr? (%cmd-lines "grep-datetime"
                                "grep -rn \"(liii date-time\" tools/pi/liii || true"
                                60
                              ) ;%cmd-lines
                  "tools/pi/liii/pi/time.scm"
                ) ;%has-substr?
    ) ;check-true
    (for-each
      (lambda (f) (check-true (>= (%grep-count "pi-current-time-seconds" f) 1)))
      %seconds-consumers
    ) ;for-each
    ;; 旧实现直接依赖 (liii time) 的三个文件已不再 import
    (for-each
      (lambda (f)
        (check (length (%cmd-lines "grep-no-liii-time"
                         (string-append "grep -n \"(liii time)\" " f " || true")
                         60
                       ) ;%cmd-lines
               ) ;length
          =>
          0
        ) ;check
      ) ;lambda
      '("tools/pi/liii/pi/time.scm"
        "tools/pi/liii/pi/agent.scm"
        "tools/pi/liii/pi/session.scm")
    ) ;for-each

    (display "=== 验收点 3: ISO 8601 输出格式不变（且与旧实现等价） ===\n"
    ) ;display
    (let* ((now (pi-iso-8601-utc-now)) (year (string->number (substring now 0 4))))
      (%check-iso-8601 now)
      (check-true (and (>= year 2024) (< year 2100)))
      (check-true
        (and (>= (string->number (substring now 5 7)) 1)
          (<= (string->number (substring now 5 7)) 12)
        ) ;and
      ) ;check-true
      (check-true
        (and (>= (string->number (substring now 8 10)) 1)
          (<= (string->number (substring now 8 10)) 31)
        ) ;and
      ) ;check-true
      (let* ((millis (string->number (substring now 20 23)))
             (before (timestamp->utc-posix-time (current-utc-timestamp)))
             (parsed (timestamp->utc-posix-time (iso-8601->timestamp now)))
             (after (timestamp->utc-posix-time (current-utc-timestamp)))
            ) ;
        (check-true (and (>= millis 0) (<= millis 999)))
        (check-true (exact? parsed))
        ;; 解析结果与当前 UTC 时间一致（允许秒级边界滑动）
        (check-true (>= parsed (- before 1)))
        (check-true (<= parsed (+ after 1)))
        ;; 毫秒字段与解析出的小数秒一致（固定三位、无精度丢失）
        (check
          (floor (* 1000 (- parsed (floor parsed))))
          =>
          millis
        ) ;check
      ) ;let*
      ;; 与迁移前旧实现（(liii time) 路径）比对：同一时间窗口内取值一致
      (let* ((legacy-before (%legacy-iso-utc-now))
             (new-ms (%iso->millis now))
             (legacy-after (%legacy-iso-utc-now))
             (legacy-before-ms (%iso->millis legacy-before))
             (legacy-after-ms (%iso->millis legacy-after))
            ) ;
        (check-true (>= new-ms (- legacy-before-ms 1000)))
        (check-true (<= new-ms (+ legacy-after-ms 1000)))
      ) ;let*
    ) ;let*

    (display "=== 验收点 4: 毫秒时间戳精确整数且量级正确（与旧实现等价） ===\n"
    ) ;display
    (let* ((ms (pi-current-time-millis)) (ms2 (pi-current-time-millis)))
      (check-true (and (number? ms) (exact? ms) (integer? ms)))
      (check-true (and (> ms 1700000000000) (< ms 2000000000000)))
      (check-true (>= ms2 ms))
      ;; 与迁移前旧实现（time-second * 1000 + time-nanosecond / 1e6）等价
      (let* ((legacy-before (%legacy-millis)) (legacy-after (%legacy-millis)))
        (check-true (>= ms (- legacy-before 1000)))
        (check-true (<= ms (+ legacy-after 1000)))
      ) ;let*
    ) ;let*

    (display "=== 验收点 5: 整秒时间戳（流式空闲超时口径）与旧实现等价 ===\n"
    ) ;display
    (let* ((secs-before (pi-current-time-seconds))
           (ms (pi-current-time-millis))
           (secs-after (pi-current-time-seconds))
          ) ;
      (check-true (and (exact? secs-before) (integer? secs-before)))
      (check-true (and (> secs-before 1700000000) (< secs-before 2000000000)))
      ;; 采样时刻夹在两次整秒读数之间：
      ;; secs-before*1000 <= millis < (secs-after+1)*1000
      (check-true (<= (* secs-before 1000) ms))
      (check-true (< ms (* (+ secs-after 1) 1000)))
      (check-true (>= secs-after secs-before))
      ;; 与迁移前旧实现（(time-second (current-time))）等价
      (let* ((legacy-before (%legacy-seconds)) (legacy-after (%legacy-seconds)))
        (check-true (>= secs-before (- legacy-before 1)))
        (check-true (<= secs-before (+ legacy-after 1)))
      ) ;let*
    ) ;let*

    (display "=== 验收点 6a: 时间函数与消费方单元测试 ===\n")
    (let loop
      ((files '("tools/pi/tests/liii/pi/pi-iso-8601-utc-now-test.scm"
                "tools/pi/tests/liii/pi/pi-current-time-millis-test.scm"
                "tools/pi/tests/liii/pi/pi-current-time-seconds-test.scm"
                "tools/pi/tests/liii/pi/kimi/stream/kimi-request-stream-test.scm"
                "tools/pi/tests/liii/pi-agent-test.scm"
                "tools/pi/tests/liii/pi-session-test.scm"
                "tools/pi/tests/liii/pi/pi-event-test.scm"
                "tools/pi/tests/liii/pi/pi-event-session-test.scm"
                "tools/pi/tests/liii/pi/compact/")
       ) ;files
       (i 0)
      ) ;
      (unless (null? files)
        (check (%ok? (string-append "test-" (number->string i))
                 (string-append "timeout 120 bin/gf test " (car files))
                 150
               ) ;%ok?
          =>
          0
        ) ;check
        (loop (cdr files) (+ i 1))
      ) ;unless
    ) ;let

    (display "=== 验收点 6b: 全量回归（pi-config-test 既有环境失败排除） ===\n"
    ) ;display
    (let* ((res (%capture "full" "timeout 290 bin/gf test tools/pi/tests/liii/" 300))
           (rc (car res))
           (out (cdr res))
           (parts (string-split out "Failed Test Files:"))
           (failed
             (if (> (length parts) 1) (%lines (car (reverse parts))) '())
           ) ;failed
          ) ;
      ;; 0 = 全绿；255 = 仅剩既有 pi-config-test 环境失败
      (check-true (or (zero? rc) (= rc 255)))
      (check-true
        (or (null? failed)
          (and (= (length failed) 1) (string-contains? (car failed) "pi-config-test.scm"))
        ) ;or
      ) ;check-true
      (check-true (string-contains? out "Total:"))
      (check-true (string-contains? out "Passed:"))
    ) ;let*
  ) ;lambda
  (lambda () (%cleanup!))
) ;dynamic-wind

(check-report)
