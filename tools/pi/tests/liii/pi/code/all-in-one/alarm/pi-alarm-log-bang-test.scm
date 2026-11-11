;; pi-alarm-log!
;; 向 alarm.log 安全追加单行异常汇总记录。
;;
;; 语法
;; ----
;; (pi-alarm-log! log-type provider model err-code message . opt-config-dir)
;;
;; 参数
;; ----
;; log-type : (or string symbol)
;; 报警类型，如 FAIL、SWITCH、REWIND、EXHAUSTED。
;; provider : (or string symbol #f)
;; 供应商名称，为 #f 时写 "none"。
;; model : (or string symbol #f)
;; 模型名称，为 #f 时写 "none"。
;; err-code : any
;; 错误码，数字以十进制输出，非数字通过 object->string 输出。
;; message : (or string #f)
;; 错误描述信息，双引号将被替换为单引号，#f 时输出空字符串。
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，用于指定 alarm.log 所在位置。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (scheme file)
  (liii check)
  (liii date-time)
  (liii path)
  (liii shutil)
  (liii string)
  (liii os)
  (liii pi code all-in-one alarm)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-alarm-bang-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

;; 1. pi-alarm-log! 自动递归建目录并按格式写入
(let* ((nested-dir (path->string (path-join TMP-DIR "nested" "sub" "conf")))
       (log-file (resolve-alarm-log-path nested-dir))
      ) ;
  (check-false (path-exists? nested-dir))
  (pi-alarm-log! "FAIL" "kimi1" "kimi-k3" 401 "Invalid \"API\" Key" nested-dir)
  (check-true (path-dir? (path nested-dir)))
  (check-true (path-file? (path log-file)))
  (let* ((content (path-read-text log-file))
         (first-close (string-index content #\]))
         (ts (substring content 1 first-close))
         (rest-line (substring content (+ first-close 1) (string-length content)))
        ) ;
    ;; TIMESTAMP 使用机器本地时区，ISO-8601 带 UTC 偏移（UTC 环境为 Z）
    (check (char=? (string-ref ts 4) #\-) => #t)
    (check (char=? (string-ref ts 7) #\-) => #t)
    (check (char=? (string-ref ts 10) #\T) => #t)
    ;; 与机器当前时区的小时和 UTC 偏移一致
    (let ((local-now (timestamp->iso-8601 (current-system-timestamp))))
      (check (substring ts 0 13) => (substring local-now 0 13))
      (if (string-ends? local-now "Z")
        (check-true (string-ends? ts "Z"))
        (check (substring ts (- (string-length ts) 6) (string-length ts))
          =>
          (substring local-now (- (string-length local-now) 6) (string-length local-now))
        ) ;check
      ) ;if
    ) ;let
    (check-true
      (or (string-ends? ts "Z")
        (and (> (string-length ts) 6)
          (let ((sign (string-ref ts (- (string-length ts) 6))))
            (or (char=? sign #\+) (char=? sign #\-))
          ) ;let
          (char=? (string-ref ts (- (string-length ts) 3)) #\:)
        ) ;and
      ) ;or
    ) ;check-true
    ;; 双引号替换为单引号，结构格式严格匹配
    (check rest-line
      =>
      " [FAIL] provider=kimi1 model=kimi-k3 error=401 message=\"Invalid 'API' Key\"\n"
    ) ;check
  ) ;let*
) ;let*

;; 2. TYPE 覆盖 FAIL、SWITCH、REWIND、EXHAUSTED 与符号参数
(let* ((type-dir (path->string (path-join TMP-DIR "types-conf")))
       (log-file (resolve-alarm-log-path type-dir))
      ) ;
  (pi-alarm-log! "SWITCH" "p-switch" "m-switch" 429 "switch message" type-dir)
  (pi-alarm-log! "REWIND" "p-rewind" "m-rewind" 0 "rewind message" type-dir)
  (pi-alarm-log! "EXHAUSTED" "p-ex" "m-ex" 503 "exhausted message" type-dir)
  (pi-alarm-log! 'FAIL "p-sym" "m-sym" 500 "symbol fail" type-dir)
  (let ((content (path-read-text log-file)))
    (check-true (string-contains? content
                  " [SWITCH] provider=p-switch model=m-switch error=429 message=\"switch message\"\n"
                ) ;string-contains?
    ) ;check-true
    (check-true (string-contains? content
                  " [REWIND] provider=p-rewind model=m-rewind error=0 message=\"rewind message\"\n"
                ) ;string-contains?
    ) ;check-true
    (check-true (string-contains? content
                  " [EXHAUSTED] provider=p-ex model=m-ex error=503 message=\"exhausted message\"\n"
                ) ;string-contains?
    ) ;check-true
    (check-true (string-contains? content
                  " [FAIL] provider=p-sym model=m-sym error=500 message=\"symbol fail\"\n"
                ) ;string-contains?
    ) ;check-true
  ) ;let
) ;let*

;; 3. 边界参数分支：#f 为 none，非数字错误码 object->string，message 为 #f
(let* ((edge-dir (path->string (path-join TMP-DIR "edge-conf")))
       (log-file (resolve-alarm-log-path edge-dir))
      ) ;
  (pi-alarm-log! "FAIL" #f #f 'timeout #f edge-dir)
  (pi-alarm-log! "SWITCH" 'kimi 'k3 "network-error" "no quote" edge-dir)
  (let ((content (path-read-text log-file)))
    (check-true (string-contains? content
                  " [FAIL] provider=none model=none error=timeout message=\"\"\n"
                ) ;string-contains?
    ) ;check-true
    (check-true (string-contains? content
                  " [SWITCH] provider=kimi model=k3 error=\"network-error\" message=\"no quote\"\n"
                ) ;string-contains?
    ) ;check-true
  ) ;let
) ;let*

;; 4. 连续多次写入：旧行逐字保留、新行追加在末尾
(let* ((append-dir (path->string (path-join TMP-DIR "append-conf")))
       (log-file (resolve-alarm-log-path append-dir))
      ) ;
  (pi-alarm-log! "FAIL" "provA" "modA" 401 "line 1" append-dir)
  (let ((c1 (path-read-text log-file)))
    (pi-alarm-log! "SWITCH" "provB" "modB" 429 "line 2" append-dir)
    (let ((c2 (path-read-text log-file)))
      (check-true (string-starts? c2 c1))
      (pi-alarm-log! "REWIND" "provC" "modC" 500 "line 3" append-dir)
      (let ((c3 (path-read-text log-file)))
        (check-true (string-starts? c3 c2))
        (let ((lines (string-split (string-trim-right c3 #\newline) "\n")))
          (check (length lines) => 3)
          (check-true (string-contains? (list-ref lines 0) "line 1"))
          (check-true (string-contains? (list-ref lines 1) "line 2"))
          (check-true (string-contains? (list-ref lines 2) "line 3"))
        ) ;let
      ) ;let
    ) ;let
  ) ;let
) ;let*

;; 清理临时目录
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
