(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii path)
  (liii os)
  (liii path stat)
  (scheme time)
  (liii tui)
  (liii pi code modes config app)
) ;import

(check-set-mode! 'report-failed)

;; pi-config-run：ranger 风格三列配置 TUI 入口。
;; q / Escape / Ctrl+C 退出并返回 0；
;; space 启停并即刻落盘；Enter（模型列）设默认模型；v 打开只读覆盖层。

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define %dir-counter 0)

;; 每个用例在临时目录副本上跑，避免写坏仓库夹具

(define (make-dir name)
  (set! %dir-counter (+ %dir-counter 1))
  (let* ((src (path-join (path FIXTURE) (path "config-tui")))
         (dst
           (path-join (path-temp-dir)
             (string-append "pi-config-app-"
               name
               "-"
               (number->string (current-second))
               "-"
               (number->string %dir-counter)
             ) ;string-append
           ) ;path-join
         ) ;dst
        ) ;
    (define (copy-tree s d)
      (unless (path-exists? d)
        (path-mkdir d :parents #t :exist-ok #t)
      ) ;unless
      (for-each
        (lambda (name)
          (let ((sp (path-join s (path name))) (dp (path-join d (path name))))
            (if (path-stat-dir? (path-stat (path->string sp)))
              (copy-tree sp dp)
              (path-copy sp dp)
            ) ;if
          ) ;let
        ) ;lambda
        (vector->list (listdir (path->string s)))
      ) ;for-each
    ) ;define
    (copy-tree src dst)
    (path->string dst)
  ) ;let*
) ;define

(define (read-file dir rel)
  (path-read-text (path (string-append dir "/" rel)))
) ;define

(define (run dir keys)
  (let ((vt (make-virtual-terminal :width 120 :height 24)))
    (for-each (lambda (k) (virtual-terminal-push-key! vt k)) keys)
    (pi-config-run dir :terminal vt :env '())
  ) ;let
) ;define

;; 1. q 退出配置并进入聊天（返回 'chat）
(let ((vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'q)
  (check (pi-config-run (make-dir "quit") :terminal vt :env '()) => 'chat)
) ;let

;; 2. Escape 直接退出（返回 'quit）
(let ((vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (check (pi-config-run (make-dir "escape") :terminal vt :env '()) => 'quit)
) ;let

;; 3. 上下移动、进入模型列后退出（不应抛错）
(check (run (make-dir "nav") (list 'down 'right 'down 'q)) => 'chat)

;; 4. 渲染后屏幕包含三列关键内容
(let ((dir (make-dir "render")) (vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'q)
  (pi-config-run dir :terminal vt :env '())
  (let ((screen (string-join (virtual-terminal-screen vt) "\n")))
    (check (string-contains screen "providers") => #t)
    (check (string-contains screen "kimi-coding") => #t)
  ) ;let
) ;let

;; 5. v 打开只读覆盖层，关闭后仍可退出（覆盖层不写盘）
(let* ((dir (make-dir "view")) (before (read-file dir "models/kimi-coding.json")))
  (check (run dir (list 'right 'v 'q 'q)) => 'chat)
  (check (read-file dir "models/kimi-coding.json") => before)
) ;let*

;; 5b. 覆盖层内滚动（j/k/g/G）后再关闭并退出：不应崩溃
(check (run (make-dir "view-scroll") (list 'v "j" "j" 'down "G" "g" 'up 'q 'q))
  =>
  'chat
) ;check

;; 6. 空格禁用默认 provider：providers.json 移除该 id，并级联默认模型
(let* ((dir (make-dir "toggle-provider"))
       (providers (read-file dir "models/providers.json"))
      ) ;
  (check (string-contains providers "kimi-coding") => #t)
  ;; 光标初始落在默认 provider（kimi-coding）上
  (check (run dir (list " " 'q)) => 'chat)
  (let ((after (read-file dir "models/providers.json")))
    (check (string-contains after "kimi-coding") => #f)
    (check (string-contains after "google") => #t)
  ) ;let
  ;; 默认模型已级联到别处
  (check (string-contains (read-file dir "settings.json") "\"kimi-coding\"")
    =>
    #f
  ) ;check
) ;let*

;; 7. Enter 在模型列 = 设为默认模型（写 settings.json，其余键保留）
(let* ((dir (make-dir "set-default")) (settings (read-file dir "settings.json")))
  (check (string-contains settings "\"defaultProvider\": \"kimi-coding\"") => #t)
  ;; 光标移到 custom-kimi（第 3 个 provider），进入模型列，Enter
  (check (run dir (list 'down 'down 'right 'enter 'q)) => 'chat)
  (let ((after (read-file dir "settings.json")))
    (check (string-contains after "\"defaultProvider\": \"custom-kimi\"") => #t)
    (check (string-contains after "\"defaultModel\": \"m1\"") => #t)
    ;; 未知键（httpProxy）保留
    (check (string-contains after "httpProxy") => #t)
  ) ;let
) ;let*

;; 8. Enter 在 provider 列 = 进入模型列（不写 settings.json）
(let* ((dir (make-dir "enter-provider")) (settings (read-file dir "settings.json")))
  (check (run dir (list 'enter 'q)) => 'chat)
  (check (read-file dir "settings.json") => settings)
) ;let*

;; 9. 被禁用 provider 的模型行按空格：只提示、不落盘
(let* ((dir (make-dir "toggle-disabled-provider"))
       (providers (read-file dir "models/providers.json"))
       (vt (make-virtual-terminal :width 120 :height 24))
      ) ;
  ;; 光标先移到第一个被禁用的 provider（白名单之后的那一段）
  (for-each (lambda (k) (virtual-terminal-push-key! vt k))
    (list 'down 'down 'down 'right " " 'q)
  ) ;for-each
  (check (pi-config-run dir :terminal vt :env '()) => 'chat)
  (check (read-file dir "models/providers.json") => providers)
) ;let*

;; 10. 缩窗自适应：窗口高度变化后重绘帧高自动收缩且方向键正常
(let* ((dir (make-dir "resize")) (vt (make-virtual-terminal :width 120 :height 30)))
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-resize! vt 100 18)
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt 'q)
  (check (pi-config-run dir :terminal vt :env '()) => 'chat)
  (let ((screen (virtual-terminal-screen vt)))
    (check (length screen) => 18)
  ) ;let
) ;let*

(check-report)
