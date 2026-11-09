(import (scheme base)
        (liii check)
        (liii string)
        (liii path)
        (liii os)
        (liii pi code cli windows-terminal))

(check-set-mode! 'report-failed)

(define %fixtures-dir
  (cond
    ((path-exists? (path "tests/fixtures/windows-terminal"))
     "tests/fixtures/windows-terminal")
    ((path-exists? (path "tools/pi/tests/fixtures/windows-terminal"))
     "tools/pi/tests/fixtures/windows-terminal")
    (else "../../tools/pi/tests/fixtures/windows-terminal")))

(define (fixture-path name)
  (path->string (path-join %fixtures-dir name)))

;; 1. 非 Windows、无 WT_SESSION、非 TTY 零副作用
(check (pi-windows-terminal-adapt-interactive!
         :is-windows #f
         :wt-session "dummy"
         :stdin-tty #t
         :stdout-tty #t)
       => 'not-active)

(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session #f
         :stdin-tty #t
         :stdout-tty #t)
       => 'not-active)

(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session "dummy"
         :stdin-tty #f
         :stdout-tty #t)
       => 'not-active)

(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session "dummy"
         :stdin-tty #t
         :stdout-tty #f)
       => 'not-active)

;; 2. 找不到配置文件
(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session "dummy"
         :stdin-tty #t
         :stdout-tty #t
         :settings-path "/non/existent/path/settings.json")
       => 'read-failed)

;; 3. 已是 unbound 或自定义键位：不弹窗确认、不写入文件
(let ((prompt-called? #f))
  (check (pi-windows-terminal-adapt-interactive!
           :is-windows #t
           :wt-session "dummy"
           :stdin-tty #t
           :stdout-tty #t
           :settings-path (fixture-path "already-unbound.jsonc")
           :confirm-proc (lambda () (set! prompt-called? #t) #t))
         => 'already-unbound)
  (check-false prompt-called?))

(let ((prompt-called? #f))
  (check (pi-windows-terminal-adapt-interactive!
           :is-windows #t
           :wt-session "dummy"
           :stdin-tty #t
           :stdout-tty #t
           :settings-path (fixture-path "custom-binding.jsonc")
           :confirm-proc (lambda () (set! prompt-called? #t) #t))
         => 'user-custom)
  (check-false prompt-called?))

;; 4. 冲突且用户同意：写回配置、保留备份、幂等
(let* ((tmp-dir (path-join (path-temp-dir) (string-append "gf-wt-adapt-" (number->string (getpid)))))
       (tmp-settings (path-join tmp-dir "settings.json"))
       (tmp-bak (path-join tmp-dir "settings.json.bak")))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-copy (fixture-path "default-binding.jsonc") (path->string tmp-settings))

  (let ((prompt-called? #f))
    ;; 首次执行：用户同意
    (check (pi-windows-terminal-adapt-interactive!
             :is-windows #t
             :wt-session "dummy"
             :stdin-tty #t
             :stdout-tty #t
             :settings-path (path->string tmp-settings)
             :confirm-proc (lambda () (set! prompt-called? #t) #t))
           => 'fixed)
    (check-true prompt-called?)
    ;; 验证 .bak 文件存在
    (check-true (path-exists? tmp-bak))
    ;; 验证 settings.json 已更新且包含 unbound
    (let ((content (path-read-text tmp-settings)))
      (check (wt-detect-alt-enter-status content) => 'already-unbound)
      (check-true (string-contains? content "unbound"))))

  ;; 再次执行：幂等，不再弹窗也不再写入
  (let ((prompt-called? #f))
    (check (pi-windows-terminal-adapt-interactive!
             :is-windows #t
             :wt-session "dummy"
             :stdin-tty #t
             :stdout-tty #t
             :settings-path (path->string tmp-settings)
             :confirm-proc (lambda () (set! prompt-called? #t) #t))
           => 'already-unbound)
    (check-false prompt-called?))

  ;; 清理
  (path-unlink tmp-settings #t)
  (path-unlink tmp-bak #t))

;; 5. 冲突且用户拒绝：不修改文件、不写 .bak
(let* ((tmp-dir (path-join (path-temp-dir) (string-append "gf-wt-decline-" (number->string (getpid)))))
       (tmp-settings (path-join tmp-dir "settings.json"))
       (tmp-bak (path-join tmp-dir "settings.json.bak")))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-copy (fixture-path "default-binding.jsonc") (path->string tmp-settings))
  (let ((orig-text (path-read-text tmp-settings))
        (prompt-called? #f))
    (check (pi-windows-terminal-adapt-interactive!
             :is-windows #t
             :wt-session "dummy"
             :stdin-tty #t
             :stdout-tty #t
             :settings-path (path->string tmp-settings)
             :confirm-proc (lambda () (set! prompt-called? #t) #f))
           => 'declined)
    (check-true prompt-called?)
    ;; 原文件未改变，bak 未生成
    (check (path-read-text tmp-settings) => orig-text)
    (check-false (path-exists? tmp-bak)))
  (path-unlink tmp-settings #t))

;; 6. 写回失败保护：原文件不被破坏，程序不崩溃，返回 write-failed
(let* ((tmp-dir (path-join (path-temp-dir) (string-append "gf-wt-ro-" (number->string (getpid)))))
       (tmp-settings (path-join tmp-dir "settings.json")))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-copy (fixture-path "default-binding.jsonc") (path->string tmp-settings))
  (let ((orig-text (path-read-text tmp-settings)))
    (catch #t
      (lambda ()
        (g_chmod (path->string tmp-dir) #o500)
        (let ((res (pi-windows-terminal-adapt-interactive!
                     :is-windows #t
                     :wt-session "dummy"
                     :stdin-tty #t
                     :stdout-tty #t
                     :settings-path (path->string tmp-settings)
                     :confirm-proc (lambda () #t))))
          (check res => 'write-failed)
          (g_chmod (path->string tmp-dir) #o700)
          (check (path-read-text tmp-settings) => orig-text)))
      (lambda args
        (g_chmod (path->string tmp-dir) #o700)
        #f)))
  (path-unlink tmp-settings #t))

(check-report)
