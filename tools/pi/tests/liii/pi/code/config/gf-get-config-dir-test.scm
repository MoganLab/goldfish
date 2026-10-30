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
;; 返回 Goldfish/pi 的 JSON 配置文件目录（path 对象，绝对路径）。
;;
;; 语法
;; ----
;; (gf-get-config-dir)
;; (gf-get-config-dir dir)
;;
;; 参数
;; ----
;; dir : string?（可选）
;; 显式的配置目录。传入非空字符串时直接作为配置目录，优先级最高。
;;
;; 返回值
;; ----
;; path?
;; 绝对路径的 path 对象。按以下优先级确定目录：
;; 1. 显式参数 dir
;; 2. 环境变量 GF_CONFIG_DIR
;; 3. 环境变量 PI_CODING_AGENT_DIR
;; 4. ~/.gf/agent（该目录存在时）
;; 5. ~/.pi/agent（兜底）
;;
;; 注意
;; ----
;; 该目录只存放 settings.json / auth.json / models.json / models-store.json /
;; multikey.json / web-search.json 等 JSON 配置文件；会话目录 sessions/ 与扩展目录
;; extensions/ 仍归属 agent 目录，由 pi-get-agent-dir 返回。
;; 参数与环境变量为空串（或仅空白）时视为未提供，继续走下一优先级。
;;
;; 示例
;; ----
;; (gf-get-config-dir)                  ;=> #<path ~/.gf/agent>
;; (gf-get-config-dir "/tmp/gf-agent")   ;=> #<path /tmp/gf-agent>

(define (%normalize dir)
  (if (os-windows?)
    (string-replace (path->string dir) "\\" "/")
    (path->string dir)
  ) ;if
) ;define

;; 用系统临时目录派生绝对路径，避免在 Windows 上把 "/tmp/xxx" 解析成 "D:/tmp/xxx"

(define (%abs-temp name)
  (path->string (path-join (path-temp-dir) name))
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
  (when (os-windows?)
    (putenv "USERPROFILE" (path->string home))
  ) ;when
  (check (%normalize (gf-get-config-dir))
    =>
    (string-append (%normalize (path-join home ".gf" "agent")))
  ) ;check
  ;; 1b. ~/.gf/agent 不存在：回退 ~/.pi/agent
  (path-rmdir (path-join home ".gf" "agent"))
  (check (%normalize (gf-get-config-dir))
    =>
    (string-append (%normalize (path-join home ".pi" "agent")))
  ) ;check
  (if saved-home (putenv "HOME" saved-home) (unsetenv "HOME"))
  (when (os-windows?)
    (if saved-profile (putenv "USERPROFILE" saved-profile) (unsetenv "USERPROFILE"))
  ) ;when
  (if saved-gf (putenv "GF_CONFIG_DIR" saved-gf) (unsetenv "GF_CONFIG_DIR"))
  (if saved-pi
    (putenv "PI_CODING_AGENT_DIR" saved-pi)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (rmtree (path->string home) #t)
) ;let*

;; 2. GF_CONFIG_DIR 环境变量覆盖
(let* ((saved (get-environment-variable "GF_CONFIG_DIR"))
       (override (%abs-temp "gf-config-dir-override"))
      ) ;
  (putenv "GF_CONFIG_DIR" override)
  (check (%normalize (gf-get-config-dir)) => (%normalize override))
  (if saved (putenv "GF_CONFIG_DIR" saved) (unsetenv "GF_CONFIG_DIR"))
) ;let*

;; 3. PI_CODING_AGENT_DIR 环境变量覆盖（兼容既有约定）
(let* ((saved (get-environment-variable "PI_CODING_AGENT_DIR"))
       (override (%abs-temp "pi-config-dir-override"))
      ) ;
  (putenv "PI_CODING_AGENT_DIR" override)
  (check (%normalize (gf-get-config-dir)) => (%normalize override))
  (if saved (putenv "PI_CODING_AGENT_DIR" saved) (unsetenv "PI_CODING_AGENT_DIR"))
) ;let*

;; 4. 显式目录参数优先于环境变量
(let* ((saved (get-environment-variable "GF_CONFIG_DIR"))
       (override (%abs-temp "gf-config-dir-override"))
       (explicit (%abs-temp "explicit-config-dir"))
      ) ;
  (putenv "GF_CONFIG_DIR" override)
  (check (%normalize (gf-get-config-dir explicit)) => (%normalize explicit))
  (if saved (putenv "GF_CONFIG_DIR" saved) (unsetenv "GF_CONFIG_DIR"))
) ;let*

;; 5. 返回值是绝对路径对象
(check-true (path? (gf-get-config-dir)))
(check-true (path-absolute? (gf-get-config-dir)))

(check-report)
