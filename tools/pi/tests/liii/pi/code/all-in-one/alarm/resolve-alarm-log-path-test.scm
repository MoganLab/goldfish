;; resolve-alarm-log-path
;; 解析当前按天日志文件的完整路径。
;;
;; 语法
;; ----
;; (resolve-alarm-log-path . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录；未提供或为 #f 时使用默认的 ~/.gf/agent/logs。
;;
;; 返回值
;; ----
;; string
;; 形如 <配置目录>/logs/pi-YYYY-MM-DD.log 的路径字符串。

(import (scheme base)
  (liii check)
  (liii date-time)
  (liii os)
  (liii path)
  (liii string)
  (liii pi code all-in-one alarm)
) ;import

(check-set-mode! 'report-failed)

(define TODAY (date->iso-8601 (timestamp-date (current-system-timestamp))))

(define LOG-NAME (string-append "pi-" TODAY ".log"))

;; 1. 未传配置目录时返回用户主目录下 .gf/agent/logs/pi-<日期>.log
(let* ((default-path (resolve-alarm-log-path))
       (expected-default (path->string (path-join (path-home) ".gf" "agent" "logs" LOG-NAME))
       ) ;expected-default
      ) ;
  (check default-path => expected-default)
  ;; 2. 可选参数为 #f 时仍走默认路径
  (check (resolve-alarm-log-path #f) => expected-default)
) ;let*

;; 3. 显式传入临时配置目录时返回该目录下 logs/pi-<日期>.log
(let* ((custom-dir
         (path-join (path-temp-dir)
           (string-append "pi-alarm-path-test-" (number->string (getpid)))
         ) ;path-join
       ) ;custom-dir
      ) ;
  (check (resolve-alarm-log-path custom-dir)
    =>
    (path->string (path-join custom-dir "logs" LOG-NAME))
  ) ;check
  (check (resolve-alarm-log-path (path->string custom-dir))
    =>
    (path->string (path-join custom-dir "logs" LOG-NAME))
  ) ;check
) ;let*

(check-report)
