;; all-in-one-provider-available?
;; 检查元供应商在给定配置目录下是否有效可用。
;;
;; 语法
;; ----
;; (all-in-one-provider-available? . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时使用默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; boolean
;; 存在有效配置且至少包含一条链时返回 #t，否则返回 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii os)
  (liii pi all_in_one provider)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-avail-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (%write-cfg content)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
  (path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
  (let ((cfg-file (path-join TMP-DIR "all-in-one.json")))
    (call-with-output-file (path->string cfg-file) (lambda (p) (display content p)))
  ) ;let
) ;define

;; 1. 无文件返回 #f
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(check (all-in-one-provider-available? TMP-DIR) => #f)

;; 2. 无链时返回 #f
(%write-cfg "{\"provider\": \"all-in-one\", \"chains\": []}")
(check (all-in-one-provider-available? TMP-DIR) => #f)

;; 3. 有有效链时返回 #t
(%write-cfg "{\"provider\": \"all-in-one\", \"chains\": [{\"id\": \"c1\", \"nodes\": [{\"provider\": \"k\", \"model\": \"m\"}]}]}"
) ;%write-cfg
(check (all-in-one-provider-available? TMP-DIR) => #t)

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
