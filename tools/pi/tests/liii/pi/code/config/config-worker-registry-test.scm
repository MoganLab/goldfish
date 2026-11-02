;; extension-registry-default-tool-names
;; 返回扩展注册表中声明 default-active? 的工具名列表，并验证 worker 会话中的可用性。
;;
;; 语法
;; ----
;; (extension-registry-default-tool-names registry)
;;
;; 参数
;; ----
;; registry : extension-registry
;; 扩展注册表对象。
;;
;; 返回值
;; ----
;; list
;; 默认激活的扩展工具名列表。

(import (liii check)
  (liii os)
  (liii go)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

(define CONFIG_EMPTY_DIR
  (if (file-exists? "tests/fixtures/config-empty")
    "tests/fixtures/config-empty"
    "tools/pi/tests/fixtures/config-empty"))

;; 默认注册表含声明 default-active? 的 web_search
(let ((reg (or (current-extension-registry) (default-extension-registry))))
  (check (extension-registry-default-tool-names reg) => '("web_search")))

;; TUI worker 独立 s7 会话中扩展注册表可用
(let* ((ch (go-result ()
            (extension-registry-default-tool-names (default-extension-registry))))
       (res (chan-recv! ch 10000)))
  (check (car res) => 'ok)
  (check (member "web_search" (cadr res)) => '("web_search")))

;; worker 会话中 parse-pi-args + resolve-pi-config 保留扩展默认工具
(let* ((_ (putenv "PI_CODING_AGENT_DIR" CONFIG_EMPTY_DIR))
       (ch (go-result ()
            (let* ((parsed (parse-pi-args '()))
                   (cfg (resolve-pi-config parsed (get-environment-variables))))
              (cdr (assoc 'tools cfg)))))
       (res (chan-recv! ch 30000)))
  (check (car res) => 'ok)
  (check (member "web_search" (cadr res)) => '("web_search"))
  (unsetenv "PI_CODING_AGENT_DIR"))

(check-report)
