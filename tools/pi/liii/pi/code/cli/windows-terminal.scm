;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli windows-terminal)
  (export wt-active? wt-find-settings-path jsonc->clean-json
    wt-detect-alt-enter-status wt-insert-unbound-action wt-apply-unbound-fix!
    pi-windows-terminal-adapt-interactive!
  ) ;export
  (import (scheme base)
    (scheme process-context)
    (scheme char)
    (liii base)
    (liii string)
    (liii list)
    (liii error)
    (liii os)
    (liii sys)
    (liii path)
    (liii pi code modes output)
    (liii pi code cli windows-terminal jsonc)
    (liii pi code cli windows-terminal insert)
  ) ;import
  (begin

    ;; 判断当前是否处于 Windows Terminal 交互式终端会话
    (define* (wt-active? (is-windows (os-windows?))
               (wt-session (get-environment-variable "WT_SESSION"))
               (stdin-tty (stdin-tty?))
               (stdout-tty (stdout-tty?))
             ) ;wt-active?
      (and is-windows
        (string? wt-session)
        (not (string-null? wt-session))
        stdin-tty
        stdout-tty
      ) ;and
    ) ;define*

    ;; 定位当前存在的 Windows Terminal settings.json 文件路径
    (define* (wt-find-settings-path (localappdata (get-environment-variable "LOCALAPPDATA")))
      (if (or (not (string? localappdata)) (string-null? localappdata))
        #f
        (let ((candidates '("Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"
                            "Packages/Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe/LocalState/settings.json"
                            "Microsoft/Windows Terminal/settings.json")
              ) ;candidates
             ) ;
          (let loop
            ((rest candidates))
            (if (null? rest)
              #f
              (let ((candidate-path (path-join (path localappdata) (car rest))))
                (if (path-exists? candidate-path)
                  (path->string candidate-path)
                  (loop (cdr rest))
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define*

    ;; 将解绑配置安全写回 settings.json（同目录 .bak 备份 + 临时文件原子替换）
    (define (wt-apply-unbound-fix! settings-path)
      (let* ((p (path settings-path)) (path-str (path->string p)))
        (unless (path-exists? p)
          (file-not-found-error (string-append "Settings file not found: " path-str))
        ) ;unless
        (let* ((text (path-read-text p)) (status (wt-detect-alt-enter-status text)))
          (if (not (eq? status 'default-conflict))
            #t
            (let* ((new-text (wt-insert-unbound-action text))
                   (bak-path (string-append path-str ".bak"))
                   (tmp-path (string-append path-str ".tmp." (number->string (getpid))))
                  ) ;
              ;; 1. 备份原配置
              (when (path-exists? (path bak-path))
                (path-unlink (path bak-path) #t)
              ) ;when
              (path-copy path-str bak-path)
              ;; 2. 写入同目录临时文件
              (catch #t
                (lambda ()
                  (path-write-text (path tmp-path) new-text)
                  ;; 3. 删除原文件并原子重命名
                  (path-unlink p #t)
                  (path-rename (path tmp-path) p)
                  #t
                ) ;lambda
                (lambda (err-type . info)
                  (when (and (not (path-exists? p)) (path-exists? (path bak-path)))
                    (catch #t (lambda () (path-copy bak-path path-str)) (lambda _ #f))
                  ) ;when
                  (catch #t
                    (lambda ()
                      (when (path-exists? (path tmp-path))
                        (path-unlink (path tmp-path) #t)
                      ) ;when
                    ) ;lambda
                    (lambda _ #f)
                  ) ;catch
                  (apply error err-type info)
                ) ;lambda
              ) ;catch
            ) ;let*
          ) ;if
        ) ;let*
      ) ;let*
    ) ;define

    (define (%default-wt-confirm)
      (let ((err-port (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display "\n[Windows Terminal] 检测到 Alt+Enter 默认绑定为全屏切换快捷键。\n"
          err-port
        ) ;display
        (display "gf pi 使用 Alt+Enter 提交 follow-up 排队任务。\n" err-port)
        (display "是否在 Windows Terminal 设置中解除 Alt+Enter 的全屏绑定？\n"
          err-port
        ) ;display
        (display "（解除后 Alt+Enter 将透传给 gf pi，F11 仍可用于全屏；此项配置将在 Windows Terminal 中持久生效）\n"
          err-port
        ) ;display
        (display "确认解除？[Y/n] " err-port)
        (flush-output-port err-port)
        (let ((line (catch #t (lambda () (read-line (current-input-port))) (lambda _ #f))))
          (if (string? line)
            (let ((trimmed (string-downcase (string-trim-both line))))
              (or (string-null? trimmed) (string=? trimmed "y") (string=? trimmed "yes"))
            ) ;let
            #f
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%handle-wt-conflict target-path confirm-proc)
      (let ((agreed? (if confirm-proc (confirm-proc) (%default-wt-confirm))))
        (if agreed?
          (catch #t
            (lambda ()
              (wt-apply-unbound-fix! target-path)
              (display-stderr "[Windows Terminal] 已解除 Alt+Enter 绑定。")
              'fixed
            ) ;lambda
            (lambda (err . info)
              (display-stderr (string-append "[Windows Terminal] 配置文件写入失败: "
                                (object->string err)
                                "，原配置已保留。"
                              ) ;string-append
              ) ;display-stderr
              'write-failed
            ) ;lambda
          ) ;catch
          (begin
            (display-stderr "[Windows Terminal] 已跳过配置。Alt+Enter 将继续触发终端全屏切换，F11 亦可用于全屏。"
            ) ;display-stderr
            'declined
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    ;; 交互式入口终端适配编排过程
    (define* (pi-windows-terminal-adapt-interactive! (is-windows (os-windows?))
               (wt-session (get-environment-variable "WT_SESSION"))
               (stdin-tty (stdin-tty?))
               (stdout-tty (stdout-tty?))
               (settings-path #f)
               (localappdata (get-environment-variable "LOCALAPPDATA"))
               (confirm-proc #f)
             ) ;pi-windows-terminal-adapt-interactive!
      (if (not (wt-active? is-windows wt-session stdin-tty stdout-tty))
        'not-active
        (let ((target-path (or settings-path (wt-find-settings-path localappdata))))
          (if (not target-path)
            'settings-not-found
            (let ((read-res
                    (catch #t (lambda () (path-read-text (path target-path))) (lambda _ #f))
                  ) ;read-res
                 ) ;
              (if (not read-res)
                'read-failed
                (let ((status (catch #t
                                (lambda () (wt-detect-alt-enter-status read-res))
                                (lambda _ 'default-conflict)
                              ) ;catch
                      ) ;status
                     ) ;
                  (case status
                   ((already-unbound) 'already-unbound)
                   ((user-custom) 'user-custom)
                   ((default-conflict) (%handle-wt-conflict target-path confirm-proc))
                   (else 'unknown-status)
                  ) ;case
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
