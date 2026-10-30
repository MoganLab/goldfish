(import (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
        (liii path)
        (liii shutil)
        (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

;; gf-get-config-dir
;; JSON 配置文件目录（path 对象）：~/.gf/agent 存在时返回该目录，否则 ~/.pi/agent。
;; 支持可选的显式目录参数与 GF_CONFIG_DIR / PI_CODING_AGENT_DIR 环境变量覆盖。

(define (%normalize dir)
  (if (os-windows?) (string-replace (path->string dir) "\\" "/") (path->string dir))
) ;define

;; 1. 无任何覆盖时按 HOME 判断：~/.gf/agent 存在则优先
(let* ((home (path-join (path-temp-dir) "gf-get-config-dir-home"))
       (saved-home (get-environment-variable "HOME"))
       (saved-profile (get-environment-variable "USERPROFILE"))
       (saved-gf (get-environment-variable "GF_CONFIG_DIR"))
       (saved-pi (get-environment-variable "PI_CODING_AGENT_DIR"))
      ) ;
  (unsetenv "GF_CONFIG_DIR")
  (unsetenv "PI_CODING_AGENT_DIR")
  ;; 1a. ~/.gf/agent 存在
  (path-mkdir (path-join home ".gf" "agent") :parents #t :exist-ok #t)
  (putenv "HOME" (path->string home))
  (when (os-windows?) (putenv "USERPROFILE" (path->string home)))
  (check (%normalize (gf-get-config-dir))
    => (string-append (%normalize (path-join home ".gf" "agent"))))
  ;; 1b. ~/.gf/agent 不存在：回退 ~/.pi/agent
  (path-rmdir (path-join home ".gf" "agent"))
  (check (%normalize (gf-get-config-dir))
    => (string-append (%normalize (path-join home ".pi" "agent"))))
  (if saved-home
    (putenv "HOME" saved-home)
    (unsetenv "HOME")
  ) ;if
  (when (os-windows?)
    (if saved-profile
      (putenv "USERPROFILE" saved-profile)
      (unsetenv "USERPROFILE")
    ) ;if
  ) ;when
  (if saved-gf
    (putenv "GF_CONFIG_DIR" saved-gf)
    (unsetenv "GF_CONFIG_DIR")
  ) ;if
  (if saved-pi
    (putenv "PI_CODING_AGENT_DIR" saved-pi)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (rmtree (path->string home) #t)
) ;let

;; 2. GF_CONFIG_DIR 环境变量覆盖
(let ((saved (get-environment-variable "GF_CONFIG_DIR")))
  (putenv "GF_CONFIG_DIR" "/tmp/gf-config-dir-override")
  (check (%normalize (gf-get-config-dir)) => (%normalize "/tmp/gf-config-dir-override"))
  (if saved
    (putenv "GF_CONFIG_DIR" saved)
    (unsetenv "GF_CONFIG_DIR")
  ) ;if
) ;let

;; 3. PI_CODING_AGENT_DIR 环境变量覆盖（兼容既有约定）
(let ((saved (get-environment-variable "PI_CODING_AGENT_DIR")))
  (putenv "PI_CODING_AGENT_DIR" "/tmp/pi-config-dir-override")
  (check (%normalize (gf-get-config-dir)) => (%normalize "/tmp/pi-config-dir-override"))
  (if saved
    (putenv "PI_CODING_AGENT_DIR" saved)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
) ;let

;; 4. 显式目录参数优先于环境变量
(let ((saved (get-environment-variable "GF_CONFIG_DIR")))
  (putenv "GF_CONFIG_DIR" "/tmp/gf-config-dir-override")
  (check (%normalize (gf-get-config-dir "/tmp/explicit-config-dir"))
    => (%normalize "/tmp/explicit-config-dir"))
  (if saved
    (putenv "GF_CONFIG_DIR" saved)
    (unsetenv "GF_CONFIG_DIR")
  ) ;if
) ;let

;; 5. 返回值是绝对路径对象
(check-true (path? (gf-get-config-dir)))
(check-true (path-absolute? (gf-get-config-dir)))

(check-report)
