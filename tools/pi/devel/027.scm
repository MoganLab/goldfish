(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme time)
  (liii check)
  (liii string)
  (liii path)
  ;; 显式导入 (srfi srfi-13)：string-prefix? 未被 (liii string) 导出，
  ;; 若不显式导入会解析到 define-library 泄漏的破损绑定并段错误（goldfish 内核 bug）
  (srfi srfi-13)
  (liii json)
  (liii subprocess)
  (liii pi)
  (liii pi bash)
  (liii pi session)
  (liii pi config)
  (liii pi event record)
  (liii pi args)
) ;import

(check-set-mode! 'report-failed)

;; [027] 会话 id 迁移至 uuid7 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

;; 会话 id 是否符合 RFC 9562 UUIDv7（36 字符、连字符位置、版本位、变体位）

(define (%uuid7-valid? id)
  (and (string? id)
    (= (string-length id) 36)
    (char=? (string-ref id 8) #\-)
    (char=? (string-ref id 13) #\-)
    (char=? (string-ref id 18) #\-)
    (char=? (string-ref id 23) #\-)
    (char=? (string-ref id 14) #\7)
    (and (memv (string-ref id 19) '(#\8 #\9 #\a #\b #\A #\B)) #t)
  ) ;and
) ;define

;; 解析 uuid7 前 48 位时间戳（毫秒）为秒

(define (%uuid7-timestamp-seconds id)
  (let* ((hex-ts (string-append (substring id 0 8) (substring id 9 13)))
         (ts-ms (string->number hex-ts 16))
        ) ;
    (/ ts-ms 1000.0)
  ) ;let*
) ;define

(display "=== 验收点 1: 单测全绿（TDD 覆盖 uuid7 格式断言） ===\n"
) ;display
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/uuid7-test.scm" 60) => 0)
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-session-test.scm" 60)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-event-session-test.scm" 60)
  =>
  0
) ;check

(display "=== 验收点 2: uuid7 函数级真实验证 ===\n")
;; 2.1 生成样本的 RFC 9562 结构断言

(define %sample-ids (map (lambda (_) (uuid7)) (iota 16)))
(for-each (lambda (id) (check-true (%uuid7-valid? id))) %sample-ids)

;; 2.2 时间戳与当前系统时钟吻合（前 48 位为 Unix 毫秒）
(for-each
  (lambda (id)
    (let ((diff (abs (- (%uuid7-timestamp-seconds id) (current-second)))))
      (check-true (< diff 120.0))
    ) ;let
  ) ;lambda
  %sample-ids
) ;for-each

;; 2.3 单调性：短时间内连续生成的 id 时间戳非递减（16 个样本取 12 对相邻比较）

(define %sorted-ts (map %uuid7-timestamp-seconds %sample-ids))
(let loop
  ((ts %sorted-ts) (ok #t))
  (cond
   ((or (null? ts) (null? (cdr ts))) (check-true ok))
   (else
     (loop (cdr ts) (and ok (>= (cadr ts) (car ts))))
   ) ;else
  ) ;cond
) ;let

;; 2.4 pi-event-session 缺省 id 为 uuid7
(let* ((ev (pi-event-session "/tmp/cwd")) (id (pi-event-session-id ev)))
  (check-true (%uuid7-valid? id))
  (check-true
    (< (abs (- (%uuid7-timestamp-seconds id) (current-second))) 120.0)
  ) ;check-true
) ;let*

;; 2.5 实现代码无 uuid4 残留（会话 id 生成两处均已迁移）
(let* ((session-src (path-read-text "tools/pi/liii/pi/session.scm"))
       (record-src (path-read-text "tools/pi/liii/pi/event/record.scm"))
      ) ;
  ;; 会话 id 生成处（sid）已迁移为 uuid7；消息 id（mid）仍用 uuid4，属合法保留
  (check-true (string-contains? session-src "(sid (uuid7))"))
  (check-true (string-contains? session-src "(mid (uuid4))"))
  (check-true (string-contains? record-src "(uuid7)"))
  ;; (liii pi) 门面重新导出 uuid7
  (check-true (string-contains? (path-read-text "tools/pi/liii/pi.scm") "uuid7"))
) ;let*

(display "=== 验收点 3: 会话文件级真实验证（jsonl 头部 + 文件名） ===\n"
) ;display
(let* ((session-root "tools/pi/tests/fixtures/tmp-027-sessions")
       (agent-dir (path->string (path-join session-root "agent")))
       (args (parse-pi-args '()))
       (cfg
         (resolve-pi-config args `(("PI_CODING_AGENT_DIR" . ,agent-dir)))
       ) ;cfg
      ) ;
  ;; 3.1 新建会话：文件名携带 uuid7 sid
  (let-values (((sfile dir) (setup-session-file args cfg "/tmp")))
    (check-true (string? sfile))
    (check-true (file-exists? sfile))
    ;; 注意：dir 为绝对路径，session-root 为相对路径，直接比较必失败，
    ;; 故先经 path-absolute 归一化为绝对路径再比较
    (check-true
      (string-prefix? (path->string (path-absolute (path session-root))) dir)
    ) ;check-true
    (let* ((fname (path-name (path sfile)))
           (sep-idx (string-index-right fname #\_))
           (dot-idx (string-index-right fname #\.))
           (file-sid (substring fname (+ sep-idx 1) dot-idx))
          ) ;
      (check-true (%uuid7-valid? file-sid))
      ;; 3.2 jsonl 头部 session 事件的 id 与文件名一致
      (let ((in (open-input-file sfile)))
        (let ((header (read-line in)))
          (let* ((j (string->json header)) (ev-id (cdr (assoc "id" j))))
            (check ev-id => file-sid)
            (check-true (%uuid7-valid? ev-id))
          ) ;let*
        ) ;let
        (close-input-port in)
      ) ;let
    ) ;let*
    ;; 3.3 会话 jsonl 可通过 read-session-history 正常解析（消息列表为空但无异常）
    (let ((hist (read-session-history sfile)))
      (check (list? hist) => #t)
    ) ;let
  ) ;let-values
  ;; 清理 fixture
  (when (file-exists? session-root)
    (%run-shell (string-append "rm -rf " session-root) 10)
  ) ;when
) ;let*

(display "=== 验收点 4: 相关既有测试与全量单测无回归 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-event-test.scm" 60) => 0)
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-cli-e2e-test.scm" 120)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/" 120) => 0)

(display "=== 验收点 5: 门禁检查 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
;; 5.3 注：验收阶段工作区有未提交改动，git diff --exit-code 不在此断言，
;; 以 fmt/check/doc 构建保证；git diff 检查由 Claude 在两次 commit 之间执行。
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)
