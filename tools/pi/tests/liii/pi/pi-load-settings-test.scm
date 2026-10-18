(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-load-settings
;; 从 settings.json 文件加载全局设置。
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(let ((settings (pi-load-settings fixture-dir)))
  (check-true (pi-settings? settings))
  (check (pi-settings-default-provider settings) => "google")
  (check (pi-settings-default-model settings) => "gemini-3.8-flash")
  (check (pi-settings-http-proxy settings) => "http://127.0.0.1:7890")
  (check (pi-settings-default-tools settings) => '("read" "bash" "edit" "write"))
  (check-true (pi-retry-config? (pi-settings-retry settings)))
  (check (cdr (assoc 'default-provider settings)) => "google")
  (check (cdr (assoc 'default-model settings)) => "gemini-3.8-flash")
  (check (cdr (assoc 'http-proxy settings)) => "http://127.0.0.1:7890")
  (let ((retry (cdr (assoc 'retry settings))))
    (check (cdr (assoc 'enabled retry)) => #t)
    (check (cdr (assoc 'max-retries retry)) => 3)
    (check (cdr (assoc 'base-delay-ms retry)) => 2000)
    (check (cdr (assoc 'max-agent-delay-ms retry)) => 60000))
  (check (cdr (assoc 'default-tools settings)) => '("read" "bash" "edit" "write")))

(let ((empty-settings (pi-load-settings "non/existent/dir")))
  (check-true (pi-settings? empty-settings))
  (check (pi-settings-default-provider empty-settings) => "google")
  (check (cdr (assoc 'default-provider empty-settings)) => "google")
  (check (cdr (assoc 'default-model empty-settings)) => "gemini-3.8-flash")
  (check (cdr (assoc 'http-proxy empty-settings)) => #f)
  (check (cdr (assoc 'default-tools empty-settings)) => #f))

(check-report)
