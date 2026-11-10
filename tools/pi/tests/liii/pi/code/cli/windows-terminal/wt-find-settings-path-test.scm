;; wt-find-settings-path
;; 按正式版、Preview、非打包安装顺序查找实际存在的 Windows Terminal 配置。
;;
;; 语法
;; ----
;; (wt-find-settings-path localappdata) -> string | #f
;;
;; 参数
;; ----
;; localappdata : string | #f
;; Windows LOCALAPPDATA 目录；为空时不查找。
;;
;; 返回值
;; ----
;; string | #f
;; 返回首个存在的 settings.json 路径，不存在时返回 #f。
;;

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii os)
  (liii pi code cli windows-terminal)
) ;import

(check-set-mode! 'report-failed)

;; 1. 空或无效 localappdata 返回 #f
(check-false (wt-find-settings-path #f))
(check-false (wt-find-settings-path ""))

;; 2. 模拟 LOCALAPPDATA 目录下的各种安装形态
(let* ((tmp-base
         (path-join (path-temp-dir)
           (string-append "gf-wt-test-" (number->string (getpid)))
         ) ;path-join
       ) ;tmp-base
       (pkg-stable (path-join tmp-base
                     "Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState"
                   ) ;path-join
       ) ;pkg-stable
       (pkg-preview (path-join tmp-base
                      "Packages/Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe/LocalState"
                    ) ;path-join
       ) ;pkg-preview
       (local-dir (path-join tmp-base "Microsoft/Windows Terminal"))
      ) ;

  ;; 确保清理
  (define (clean-tmp!)
    (catch #t
      (lambda ()
        (when (path-exists? (path-join pkg-stable "settings.json"))
          (path-unlink (path-join pkg-stable "settings.json"))
        ) ;when
        (when (path-exists? (path-join pkg-preview "settings.json"))
          (path-unlink (path-join pkg-preview "settings.json"))
        ) ;when
        (when (path-exists? (path-join local-dir "settings.json"))
          (path-unlink (path-join local-dir "settings.json"))
        ) ;when
      ) ;lambda
      (lambda _ #f)
    ) ;catch
  ) ;define

  (clean-tmp!)

  ;; 2.1 均不存在时返回 #f
  (check-false (wt-find-settings-path (path->string tmp-base)))

  ;; 2.2 仅非打包安装存在
  (path-mkdir local-dir :parents #t :exist-ok #t)
  (path-touch (path-join local-dir "settings.json"))
  (check-true (string? (wt-find-settings-path (path->string tmp-base))))
  (check-true (string-contains? (wt-find-settings-path (path->string tmp-base))
                "Microsoft/Windows Terminal"
              ) ;string-contains?
  ) ;check-true

  ;; 2.3 Preview 存在（优先级高于非打包）
  (path-mkdir pkg-preview :parents #t :exist-ok #t)
  (path-touch (path-join pkg-preview "settings.json"))
  (check-true (string-contains? (wt-find-settings-path (path->string tmp-base))
                "WindowsTerminalPreview"
              ) ;string-contains?
  ) ;check-true

  ;; 2.4 正式版存在（优先级最高）
  (path-mkdir pkg-stable :parents #t :exist-ok #t)
  (path-touch (path-join pkg-stable "settings.json"))
  (check-true (string-contains? (wt-find-settings-path (path->string tmp-base))
                "Microsoft.WindowsTerminal_8wekyb3d8bbwe"
              ) ;string-contains?
  ) ;check-true

  (clean-tmp!)
) ;let*

(check-report)
