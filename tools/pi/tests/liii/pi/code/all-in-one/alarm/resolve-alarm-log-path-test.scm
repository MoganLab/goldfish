;; resolve-alarm-log-path
;; 解析 alarm.log 的完整文件路径。
;;
;; 语法
;; ----
;; (resolve-alarm-log-path . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时返回默认的 ~/.gf/agent/alarm.log。
;;
;; 返回值
;; ----
;; string
;; alarm.log 文件的完整路径字符串。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii os)
  (liii pi code all-in-one alarm)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-alarm-path-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

;; 1. 未传配置目录时返回用户主目录下 .gf/agent/alarm.log
(let ((default-path (resolve-alarm-log-path))
      (expected-default (path->string (path-join (path-home) ".gf" "agent" "alarm.log"))
      ) ;expected-default
     ) ;
  (check default-path => expected-default)
  ;; 2. 可选参数为 #f 时仍走默认路径
  (check (resolve-alarm-log-path #f) => expected-default)
  ;; 3. 显式传入临时配置目录时返回该目录下 alarm.log
  (let ((custom-dir (path-join TMP-DIR "custom-conf")))
    (check (resolve-alarm-log-path custom-dir)
      =>
      (path->string (path-join custom-dir "alarm.log"))
    ) ;check
    (check (resolve-alarm-log-path (path->string custom-dir))
      =>
      (path->string (path-join custom-dir "alarm.log"))
    ) ;check
  ) ;let
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
