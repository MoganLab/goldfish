(import (scheme base)
  (liii check)
  (liii string)
  (liii pi ai config record)
  (liii pi ai event)
  (liii pi ai multikey)
)

(check-set-mode! 'report-failed)

;; multikey 切换日志
;; 首次选 key 和正常复用当前 key 不打日志；每次实际切换都打一条脱敏日志。
;; 默认写 stderr；TUI 通过 set-pi-multikey-logger! 转为界面卡片；
;; 单测经 rotate! 的可选 port 参数捕获日志内容。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

;; 1. 首次选择 key-1 以及正常复用 key-1 均无日志
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "auth-json-key")))
       (p (open-output-string)))
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
  (let ((out (get-output-string p)))
    (check out => "")
  ) ;let
) ;let*

;; 2. key-1 429 → key-2：按原生 pi 风格记录 provider、状态与脱敏 key
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (p (open-output-string))
       (next #f))
  (set! next (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 429 p))
  (check next => "sk-pool-beta-00002")
  (let ((out (get-output-string p)))
    (check-true (string-contains out
                  "multikey[kimi-coding]: 429 on alpha (sk-poo…0001), rotating to another key"))
    ;; 脱敏：完整 key 不出现在日志中
    (check-false (string-contains out "sk-pool-alpha-0001"))
    (check-false (string-contains out "sk-pool-beta-00002"))
  ) ;let
) ;let*

;; 3. 后续 key-2 403 → key-3 也记录日志，不只记录第一次切换
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (p (open-output-string))
       (second #f))
  (set! second (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 429 p))
  (check (pi-multikey-rotate! cfg second 403 p) => "sk-pool-delta-00004")
  (let ((out (get-output-string p)))
    (check-true (string-contains out
                  "multikey[kimi-coding]: 429 on alpha (sk-poo…0001), rotating to another key"))
    (check-true (string-contains out
                  "multikey[kimi-coding]: 403 on beta (sk-poo…0002), rotating to another key"))
    (check-false (string-contains out "sk-pool-delta-00004"))
  ) ;let
) ;let*

;; 4. 无 label 的 key 日志中显示为 key
(let* ((pool (pi-load-multikey fixture-dir "google"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "goog-key-0000000001")))
       (p (open-output-string)))
  (pi-multikey-rotate! cfg "goog-key-0000000001" 429 p)
  (check-true (string-contains (get-output-string p)
                "multikey[google]: 429 on g1 (goog-k…0001), rotating to another key"))
) ;let*

;; 5. 自定义 logger 接收同样的消息，供 TUI 转为可见卡片
(let ((pool (pi-load-multikey fixture-dir "kimi-coding"))
      (cfg #f)
      (logged '()))
  (set! cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
  (dynamic-wind
    (lambda ()
      (set-pi-multikey-logger! (lambda (msg) (set! logged (cons msg logged))))
    ) ;lambda
    (lambda ()
      (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 429)
      (check (length logged) => 1)
      (check-true (string-contains (car logged)
                    "multikey[kimi-coding]: 429 on alpha (sk-poo…0001), rotating to another key"))
    ) ;lambda
    (lambda () (set-pi-multikey-logger! #f))
  ) ;dynamic-wind
) ;let

(check-report)
