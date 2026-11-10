;; pi-windows-terminal-adapt-interactive!
;; 在交互式 Windows Terminal 会话中检测并处理 Alt+Enter 全屏快捷键冲突。
;;
;; 语法
;; ----
;; (pi-windows-terminal-adapt-interactive! :is-windows flag :wt-session session
;;   :stdin-tty flag :stdout-tty flag :settings-path path :confirm-proc proc) -> symbol
;;
;; 参数
;; ----
;; 关键字参数可注入操作系统、WT_SESSION、TTY、配置路径和确认过程，用于覆盖
;; 非目标环境、状态分类、拒绝、写回失败及幂等场景。
;;
;; 返回值
;; ----
;; symbol
;; 返回 not-active、read-failed、already-unbound、user-custom、fixed、declined
;; 或 write-failed；仅在确认冲突且用户同意后写回配置。
;;

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii os)
  (liii pi code cli windows-terminal)
) ;import

(check-set-mode! 'report-failed)

(define %fixtures-dir
  (cond ((path-exists? (path "tests/fixtures/windows-terminal"))
         "tests/fixtures/windows-terminal"
        ) ;
        ((path-exists? (path "tools/pi/tests/fixtures/windows-terminal"))
         "tools/pi/tests/fixtures/windows-terminal"
        ) ;
        (else "../../tools/pi/tests/fixtures/windows-terminal")
  ) ;cond
) ;define

(define (fixture-path name)
  (path->string (path-join %fixtures-dir name))
) ;define

;; 1. 非 Windows、无 WT_SESSION、非 TTY 零副作用
(check (pi-windows-terminal-adapt-interactive!
         :is-windows #f
         :wt-session "dummy"
         :stdin-tty  #t
         :stdout-tty #t
       ) ;pi-windows-terminal-adapt-interactive!
  =>
  'not-active
) ;check

(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session #f
         :stdin-tty  #t
         :stdout-tty #t
       ) ;pi-windows-terminal-adapt-interactive!
  =>
  'not-active
) ;check

(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session "dummy"
         :stdin-tty  #f
         :stdout-tty #t
       ) ;pi-windows-terminal-adapt-interactive!
  =>
  'not-active
) ;check

(check (pi-windows-terminal-adapt-interactive!
         :is-windows #t
         :wt-session "dummy"
         :stdin-tty  #t
         :stdout-tty #f
       ) ;pi-windows-terminal-adapt-interactive!
  =>
  'not-active
) ;check

;; 2. 找不到配置文件
(check (pi-windows-terminal-adapt-interactive!
         :is-windows    #t
         :wt-session    "dummy"
         :stdin-tty     #t
         :stdout-tty    #t
         :settings-path "/non/existent/path/settings.json"
       ) ;pi-windows-terminal-adapt-interactive!
  =>
  'read-failed
) ;check

;; 3. 已是 unbound 或自定义键位：不弹窗确认、不写入文件
(let ((prompt-called? #f))
  (check (pi-windows-terminal-adapt-interactive!
           :is-windows    #t
           :wt-session    "dummy"
           :stdin-tty     #t
           :stdout-tty    #t
           :settings-path (fixture-path "already-unbound.jsonc")
           :confirm-proc  (lambda () (set! prompt-called? #t) #t)
         ) ;pi-windows-terminal-adapt-interactive!
    =>
    'already-unbound
  ) ;check
  (check-false prompt-called?)
) ;let

(let ((prompt-called? #f))
  (check (pi-windows-terminal-adapt-interactive!
           :is-windows    #t
           :wt-session    "dummy"
           :stdin-tty     #t
           :stdout-tty    #t
           :settings-path (fixture-path "custom-binding.jsonc")
           :confirm-proc  (lambda () (set! prompt-called? #t) #t)
         ) ;pi-windows-terminal-adapt-interactive!
    =>
    'user-custom
  ) ;check
  (check-false prompt-called?)
) ;let

;; 4. 冲突且用户同意：写回配置、保留备份、幂等
(let* ((tmp-dir
         (path-join (path-temp-dir)
           (string-append "gf-wt-adapt-" (number->string (getpid)))
         ) ;path-join
       ) ;tmp-dir
       (tmp-settings (path-join tmp-dir "settings.json"))
       (tmp-bak (path-join tmp-dir "settings.json.bak"))
      ) ;
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-copy (fixture-path "default-binding.jsonc") (path->string tmp-settings))

  (let ((prompt-called? #f))
    ;; 首次执行：用户同意
    (check (pi-windows-terminal-adapt-interactive!
             :is-windows    #t
             :wt-session    "dummy"
             :stdin-tty     #t
             :stdout-tty    #t
             :settings-path (path->string tmp-settings)
             :confirm-proc  (lambda () (set! prompt-called? #t) #t)
           ) ;pi-windows-terminal-adapt-interactive!
      =>
      'fixed
    ) ;check
    (check-true prompt-called?)
    ;; 验证 .bak 文件存在
    (check-true (path-exists? tmp-bak))
    ;; 验证 settings.json 已更新且包含 unbound
    (let ((content (path-read-text tmp-settings)))
      (check (wt-detect-alt-enter-status content) => 'already-unbound)
      (check-true (string-contains? content "unbound"))
    ) ;let
  ) ;let

  ;; 再次执行：幂等，不再弹窗也不再写入
  (let ((prompt-called? #f))
    (check (pi-windows-terminal-adapt-interactive!
             :is-windows    #t
             :wt-session    "dummy"
             :stdin-tty     #t
             :stdout-tty    #t
             :settings-path (path->string tmp-settings)
             :confirm-proc  (lambda () (set! prompt-called? #t) #t)
           ) ;pi-windows-terminal-adapt-interactive!
      =>
      'already-unbound
    ) ;check
    (check-false prompt-called?)
  ) ;let

  ;; 清理
  (path-unlink tmp-settings #t)
  (path-unlink tmp-bak #t)
) ;let*

;; 5. 冲突且用户拒绝：不修改文件、不写 .bak
(let* ((tmp-dir
         (path-join (path-temp-dir)
           (string-append "gf-wt-decline-" (number->string (getpid)))
         ) ;path-join
       ) ;tmp-dir
       (tmp-settings (path-join tmp-dir "settings.json"))
       (tmp-bak (path-join tmp-dir "settings.json.bak"))
      ) ;
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-copy (fixture-path "default-binding.jsonc") (path->string tmp-settings))
  (let ((orig-text (path-read-text tmp-settings)) (prompt-called? #f))
    (check (pi-windows-terminal-adapt-interactive!
             :is-windows    #t
             :wt-session    "dummy"
             :stdin-tty     #t
             :stdout-tty    #t
             :settings-path (path->string tmp-settings)
             :confirm-proc  (lambda () (set! prompt-called? #t) #f)
           ) ;pi-windows-terminal-adapt-interactive!
      =>
      'declined
    ) ;check
    (check-true prompt-called?)
    ;; 原文件未改变，bak 未生成
    (check (path-read-text tmp-settings) => orig-text)
    (check-false (path-exists? tmp-bak))
  ) ;let
  (path-unlink tmp-settings #t)
) ;let*

;; 6. 写回失败保护：原文件不被破坏，程序不崩溃，返回 write-failed
(let* ((tmp-dir
         (path-join (path-temp-dir)
           (string-append "gf-wt-ro-" (number->string (getpid)))
         ) ;path-join
       ) ;tmp-dir
       (tmp-settings (path-join tmp-dir "settings.json"))
      ) ;
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-copy (fixture-path "default-binding.jsonc") (path->string tmp-settings))
  (let ((orig-text (path-read-text tmp-settings)))
    (catch #t
      (lambda ()
        (g_chmod (path->string tmp-dir) 320)
        (let ((res (pi-windows-terminal-adapt-interactive!
                     :is-windows    #t
                     :wt-session    "dummy"
                     :stdin-tty     #t
                     :stdout-tty    #t
                     :settings-path (path->string tmp-settings)
                     :confirm-proc  (lambda () #t)
                   ) ;pi-windows-terminal-adapt-interactive!
              ) ;res
             ) ;
          (check res => 'write-failed)
          (g_chmod (path->string tmp-dir) 448)
          (check (path-read-text tmp-settings) => orig-text)
        ) ;let
      ) ;lambda
      (lambda args (g_chmod (path->string tmp-dir) 448) #f)
    ) ;catch
  ) ;let
  (path-unlink tmp-settings #t)
) ;let*

(check-report)
