(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii tui)
  (liii go)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
  (liii pi code modes interactive session-selector)
  (liii pi code core session-list)
) ;import

(check-set-mode! 'report-failed)

;; 屏幕纯文本（去掉 ANSI 颜色码），便于断言提示文案
(define (screen-text app)
  (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
) ;define

;; footer 纯文本：会话名只应出现在 footer，聊天卡片里的历史文案不参与判断
(define (footer-text app)
  (string-join (map strip-ansi (component-render (%pi-app-footer app) 100)) "\n")
) ;define

;; 构造一条会话摘要（供选择器枚举使用）
(define (make-info path id name modified)
  (make-pi-session-info path
    id
    "/work"
    name
    "2026-10-21T08:00:00.000Z"
    modified
    1
    "msg"
    "msg"
  ) ;make-pi-session-info
) ;define

;; 1. 命令识别：仅精确匹配 /resume；/resumes 等前缀不算命令
(check-true (pi-app-resume-command? "/resume"))
(check-true (pi-app-resume-command? "/resume now"))
(check-false (pi-app-resume-command? "/resumes"))
(check-false (pi-app-resume-command? "hello /resume"))
(check-false (pi-app-resume-command? "/res"))

;; 2. 斜杠命令菜单包含 /resume，描述为 Select and resume a session；/res 可 fuzzy 命中
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (ed (pi-app-editor app)))
  (component-handle-input ed "/")
  (check (map (lambda (item) (cdr (assoc 'value item)))
           (select-list-items (pi-app-slash-command-menu app)))
    => '("model" "thinking" "name" "session" "resume"))
  (check (map (lambda (item) (cdr (assoc 'description item)))
           (select-list-items (pi-app-slash-command-menu app)))
    => '("Select model"
      "Set thinking level"
      "Get or set session name"
      "Show session statistics"
      "Select and resume a session"
      ) ;list
  )
  (editor-set-text! ed "/res")
  (check (map (lambda (item) (cdr (assoc 'value item)))
           (select-list-items (pi-app-slash-command-menu app)))
    => '("resume")))

;; 3. /resume 打开会话选择器：编辑器清空并失焦，界面出现 Resume Session 标题；
;;    命令不会被当作用户消息发给模型
(let* ((received '())
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received)))
              :resume-sessions (lambda () (values '() (lambda () '())))))
       (ed (pi-app-editor app)))
  (pi-app-submit! app "/resume")
  (check-true (session-selector? (pi-app-active-selector app)))
  (check (editor-text ed) => "")
  (check (editor-focused? ed) => #f)
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "Resume Session"))
  (check received => '())
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 4. Escape 取消选择器：无副作用（不投任务、不改会话名）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) #t)
              :resume-sessions (lambda () (values '() (lambda () '())))))
       (ed (pi-app-editor app)))
  (%pi-app-session-name-set! app "原会话名")
  (pi-app-submit! app "/resume")
  (check-true (session-selector? (pi-app-active-selector app)))
  (pi-app-handle-input! app 'escape)
  (check (pi-app-active-selector app) => #f)
  (check (editor-focused? ed) => #t)
  (check (pi-app-session-name app) => "原会话名")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 5. Ctrl+C 取消选择器：同样无副作用
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) #t)
              :resume-sessions (lambda () (values '() (lambda () '())))))
       (ed (pi-app-editor app)))
  (pi-app-submit! app "/resume")
  (check-true (session-selector? (pi-app-active-selector app)))
  (pi-app-handle-input! app 'ctrl-c)
  (check (pi-app-active-selector app) => #f)
  (check (editor-focused? ed) => #t)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 6. 选择器交互沿用 --resume：Tab 切换 Current/All、Ctrl+S 切换排序、
;;    上下键移动选择；Enter 确认后投递 (session_resume 路径) 任务
(let* ((info-a (make-info "/tmp/a.jsonl" "id-a" #f "2026-10-21T08:00:01.000Z"))
       (info-b (make-info "/tmp/b.jsonl" "id-b" "Beta" "2026-10-21T09:00:01.000Z"))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) #t)
              :resume-sessions
              (lambda () (values (list info-a info-b) (lambda () (list info-a info-b))))))
       (ed (pi-app-editor app)))
  (pi-app-submit! app "/resume")
  (let ((sel (pi-app-active-selector app)))
    (check (session-selector-scope sel) => 'current)
    (pi-app-handle-input! app 'tab)
    (check (session-selector-scope sel) => 'all)
    (pi-app-handle-input! app 'tab)
    (check (session-selector-scope sel) => 'current)
    (pi-app-handle-input! app 'ctrl-s)
    (check (session-selector-sort-mode sel) => 'relevance)
    (pi-app-handle-input! app 'down)
    (check (session-selector-selected-index sel) => 1)
    (pi-app-handle-input! app 'enter))
  ;; 确认后：选择器关闭、编辑器恢复、任务载荷为 (session_resume 目标路径)
  (check (pi-app-active-selector app) => #f)
  (check (editor-focused? ed) => #t)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => '(session_resume "/tmp/b.jsonl")))

;; 7. busy 时拒绝切换：提示稍后重试，不开选择器、不投任务
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) #t)
              :resume-sessions (lambda () (values '() (lambda () '())))))
       (q (pi-app-queue app)))
  (pi-queue-set-busy! q)
  (pi-app-submit! app "/resume")
  (check (pi-app-active-selector app) => #f)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "try /resume again later")))

;; 8. 队列中有排队消息时同样拒绝（避免切换后消息串写）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) #t)
              :resume-sessions (lambda () (values '() (lambda () '())))))
       (q (pi-app-queue app)))
  (pi-queue-enqueue! q "pending message")
  (pi-app-submit! app "/resume")
  (check (pi-app-active-selector app) => #f)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 9. --no-session 时明确拒绝：不切换会话、不创建文件
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) #t)
              :no-session? #t
              :resume-sessions (lambda () (values '() (lambda () '()))))))
  (pi-app-submit! app "/resume")
  (check (pi-app-active-selector app) => #f)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "--no-session")))

;; 10. 无会话数据源时拒绝
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t))))
  (pi-app-submit! app "/resume")
  (check (pi-app-active-selector app) => #f)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 11. /resume 带参数时提示用法，不当作普通消息发给模型
(let* ((received '())
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received)))
              :resume-sessions (lambda () (values '() (lambda () '()))))))
  (pi-app-submit! app "/resume now")
  (check received => '())
  (check (pi-app-active-selector app) => #f)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "Usage: /resume")))

;; 12. session_resumed 事件：app 与 footer 的会话名同步为所选会话名称；
;;     未命名会话清空旧名称；失败事件以错误卡片上屏
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t))))
  (%pi-app-session-name-set! app "旧会话名")
  (pi-footer-set-session-name! (%pi-app-footer app) "旧会话名")
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/x.jsonl" "新会话名"))
  (check (pi-app-session-name app) => "新会话名")
  (pi-app-render! app)
  (check-true (string-contains? (footer-text app) "新会话名"))
  (check-true (string-contains? (screen-text app) "新会话名"))
  (check-true (string-contains? (screen-text app) "Resumed session"))
  ;; 未命名会话：旧名称必须被清空，footer 不残留上一个会话名
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/y.jsonl" #f))
  (check (pi-app-session-name app) => #f)
  (pi-app-render! app)
  (check-false (string-contains? (footer-text app) "新会话名"))
  ;; 失败事件：错误卡片上屏
  (let ((before (length (pi-app-chat app))))
    (pi-app-handle-resume-event! app (list 'session_resume_error "boom"))
    (check (= (length (pi-app-chat app)) (+ before 1)) => #t)
    (pi-app-render! app)
    (check-true (string-contains? (screen-text app) "boom"))))

(check-report)
