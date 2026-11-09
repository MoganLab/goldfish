(import (scheme base)
        (liii check)
        (liii string)
        (liii path)
        (liii os)
        (liii pi code cli windows-terminal))

(check-set-mode! 'report-failed)

;; 1. 空或无效 localappdata 返回 #f
(check-false (wt-find-settings-path #f))
(check-false (wt-find-settings-path ""))

;; 2. 模拟 LOCALAPPDATA 目录下的各种安装形态
(let* ((tmp-base (path-join (path-temp-dir) (string-append "gf-wt-test-" (number->string (getpid)))))
       (pkg-stable (path-join tmp-base "Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState"))
       (pkg-preview (path-join tmp-base "Packages/Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe/LocalState"))
       (local-dir (path-join tmp-base "Microsoft/Windows Terminal")))

  ;; 确保清理
  (define (clean-tmp!)
    (catch #t
      (lambda ()
        (when (path-exists? (path-join pkg-stable "settings.json"))
          (path-unlink (path-join pkg-stable "settings.json")))
        (when (path-exists? (path-join pkg-preview "settings.json"))
          (path-unlink (path-join pkg-preview "settings.json")))
        (when (path-exists? (path-join local-dir "settings.json"))
          (path-unlink (path-join local-dir "settings.json"))))
      (lambda _ #f)))

  (clean-tmp!)

  ;; 2.1 均不存在时返回 #f
  (check-false (wt-find-settings-path (path->string tmp-base)))

  ;; 2.2 仅非打包安装存在
  (path-mkdir local-dir :parents #t :exist-ok #t)
  (path-touch (path-join local-dir "settings.json"))
  (check-true (string? (wt-find-settings-path (path->string tmp-base))))
  (check-true (string-contains? (wt-find-settings-path (path->string tmp-base)) "Microsoft/Windows Terminal"))

  ;; 2.3 Preview 存在（优先级高于非打包）
  (path-mkdir pkg-preview :parents #t :exist-ok #t)
  (path-touch (path-join pkg-preview "settings.json"))
  (check-true (string-contains? (wt-find-settings-path (path->string tmp-base)) "WindowsTerminalPreview"))

  ;; 2.4 正式版存在（优先级最高）
  (path-mkdir pkg-stable :parents #t :exist-ok #t)
  (path-touch (path-join pkg-stable "settings.json"))
  (check-true (string-contains? (wt-find-settings-path (path->string tmp-base)) "Microsoft.WindowsTerminal_8wekyb3d8bbwe"))

  (clean-tmp!))

(check-report)
