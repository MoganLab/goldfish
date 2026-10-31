;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 编辑器输入变化：识别 bash 模式并联动边框颜色
(define-library (liii pi code modes interactive app-submit)
  (export %starts-with-char? %pi-app-editor-on-change pi-app-submit!)
  (import (scheme base)
    (liii string)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive bash)
    (liii pi code modes interactive slash-command-menu)
    (liii pi code modes interactive app-model)
    (liii pi code modes interactive app-thinking)
    (liii pi code modes interactive app-session)
    (liii pi code modes interactive app-name)
    (liii pi code modes interactive app-resume)
    (liii pi code modes interactive app-config)
    (liii pi code modes interactive app-bash)
    (liii pi code modes interactive app-events)
    (liii pi code modes interactive render)
  ) ;import
  (begin

    (define (%starts-with-char? s c)
      (and (> (string-length s) 0) (char=? (string-ref s 0) c))
    ) ;define

    ;; bash 模式边框联动（对齐 pi onChange + updateEditorBorderColor）：
    ;; 文本（去前导空白后）以 ! 开头 → 绿色；否则恢复当前 thinking 级别颜色
    (define (%pi-app-editor-on-change app ed text)
      (let ((bash? (%starts-with-char? (string-trim-left text) #\!))
            (matches (pi-slash-command-matches text))
           ) ;
        (%pi-app-slash-command-menu-set! app
          (and matches (make-pi-slash-command-menu matches))
        ) ;%pi-app-slash-command-menu-set!
        (%pi-app-bash-mode?-set! app bash?)
        (editor-set-border-color! ed
          (if bash?
            pi-bash-border-color
            (pi-thinking-border-color (pi-app-thinking-level app))
          ) ;if
        ) ;editor-set-border-color!
      ) ;let
    ) ;define

    ;; 用户提交：/model 切换模型；/session 展示统计；/name 管理名称；
    ;; /resume 恢复历史会话；! 前缀执行 bash；其余消息按运行状态路由：
    ;; 空闲 → 等待队列（现有调度）；busy → steering（默认，回车）
    ;; 或 follow-up（:mode 'follow-up，alt+enter）注入运行中/下一轮任务
    (define* (pi-app-submit! app text (mode 'auto))
      (let ((clean (string-trim text)))
        (unless (string-null? clean)
          (cond ((pi-app-model-command? clean) (pi-app-submit-model! app clean))
                ((pi-app-thinking-command? clean) (pi-app-submit-thinking! app clean))
                ((pi-app-session-command? clean) (pi-app-submit-session! app))
                ((pi-app-name-command? clean) (pi-app-submit-name! app clean))
                ((pi-app-resume-command? clean) (pi-app-submit-resume! app clean))
                ((pi-app-config-command? clean) (pi-app-submit-config! app))
                (else
                  ;; bash 模式：!cmd 直接经 bash 执行（对齐 pi 的 !/!! 前缀行为）；
                  ;; 命令为空（仅 "!" 或 "!!"）时回退为普通消息提交（对齐 pi 的 fall-through）
                  (let* ((bang? (%starts-with-char? clean #\!))
                         (excluded? (and bang? (> (string-length clean) 1) (char=? (string-ref clean 1) #\!))
                         ) ;excluded?
                         (cmd-start (if excluded? 2 1))
                         (command
                           (and bang?
                             (not (string-null? (string-trim (substring clean cmd-start))))
                             (string-trim (substring clean cmd-start))
                           ) ;and
                         ) ;command
                        ) ;
                    (if command
                      (pi-app-run-bash! app command :excluded? excluded?)
                      (let ((q (pi-app-queue app)) (ed (pi-app-editor app)))
                        (cond
                          ;; busy：按模式注入运行中任务（steering / follow-up）
                          ((pi-queue-busy? q)
                           (if (eq? mode 'follow-up)
                             (pi-app-queue-follow-up! app clean)
                             (pi-app-queue-steering! app clean)
                           ) ;if
                           (editor-set-text! ed "")
                           ;; 发送新消息时把对话记录拉回末尾（恢复 follow），
                           ;; 否则用户停留在历史位置会看不到自己的新消息
                           (pi-app-scroll-bottom! app)
                           (pi-app-render! app)
                          ) ;
                          ;; 空闲：进入等待队列，沿用现有调度路径
                          (else (pi-queue-enqueue! q clean)
                            (editor-set-text! ed "")
                            (pi-app-scroll-bottom! app)
                            (pi-app-drain-queue! app)
                          ) ;else
                        ) ;cond
                      ) ;let
                    ) ;if
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;unless
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
