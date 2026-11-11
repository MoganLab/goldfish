;; make-pi-app
;; 构造并初始化交互式应用实例。
;;
;; 语法
;; ----
;; (make-pi-app . kwargs)
;;
;; 参数
;; ----
;; kwargs : any
;; 关键字参数（如 :terminal, :handler, :provider, :model, :session-file 等）。
;;
;; 返回值
;; ------
;; any
;; 初始化完成的交互式应用对象（满足 pi-app?）。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii tui)
  (liii pi code core session-manager)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 成功构造应用实例
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :provider "google" :model "gemini-2.5-flash"))
      ) ;
  (check-true (pi-app? app))
  (check (pi-app-provider app) => "google")
  (check (pi-app-model app) => "gemini-2.5-flash")
) ;let*

;; 2. 新会话（无 session-file）：聊天区为空，不凭空出现历史卡片
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (check (pi-app-chat app) => '())
) ;let*

;; 3. 带 session-file 创建应用：恢复该会话的可见历史
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-make-pi-app-history-1323")))
       (file (path->string (path-join tmp-dir "session.jsonl")))
       (vt (make-virtual-terminal :width 80 :height 24))
       (app #f)
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! file "sess-start" "2026-11-04T08:00:00.000Z" tmp-dir)
  (save-session-message! file
    "{\"role\":\"user\",\"content\":\"启动前的历史提问\"}" "m1" #f
    "2026-11-04T08:00:01.000Z"
  ) ;save-session-message!
  (save-session-message! file
    "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"启动前的历史回答\"}]}"
    "m2" "m1" "2026-11-04T08:00:02.000Z"
  ) ;save-session-message!
  (set! app (make-pi-app :terminal vt :handler (lambda (a msg) #t) :session-file file))
  (let ((plain (string-join (map strip-ansi (pi-app-transcript-lines app)) "\n")))
    (check-true (string-contains? plain "启动前的历史提问"))
    (check-true (string-contains? plain "启动前的历史回答"))
  ) ;let
  (rmtree tmp-dir)
) ;let*

(check-report)
