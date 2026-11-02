;; pi-config-run
;; gf pi config 三列配置 TUI 主循环：进入备用屏与 raw 模式后循环轮询按键、
;; 分发处理并渲染，q/Esc/Ctrl+C 退出；空闲分支检测终端尺寸变化并自动
;; 清屏重绘。本文件覆盖导航、启停、设默认模型、只读覆盖层与缩窗自适应。
;;
;; 语法
;; ----
;; (pi-config-run config-dir :terminal term :env env :nested nested)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; :terminal : terminal（可选）
;; 终端实例，缺省为标准终端；测试传入 virtual-terminal。
;; :env : alist 或 #f（可选）
;; 环境变量表；#f 时读进程环境。
;; :nested : boolean（可选）
;; 由外层 TUI（如 gf pi 的 /config）调用时为 #t，不进/退备用屏。
;;
;; 返回值
;; ------
;; symbol
;; 退出方式：q 返回 'chat，Esc/Ctrl+C 返回 'quit。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii path)
  (liii os)
  (liii path stat)
  (scheme time)
  (liii tui)
  (liii pi code modes config state)
  (liii pi code modes config layout)
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
  ;; 提示行给出拒绝原因（修复前消息只在函数内部 set!，永远不显示）
  (let ((screen (string-join (virtual-terminal-screen vt) "\n")))
    (check (string-contains screen "is disabled") => #t)
  ) ;let
) ;let*

;; 9b. 唯一可用 provider 按空格：拒绝禁用并在提示行给出原因，白名单不被改写
(let* ((dir (make-dir "toggle-last-provider")))
  (path-write-text (path (string-append dir "/models/providers.json")) "[\"google\"]")
  (path-write-text (path (string-append dir "/settings.json"))
    "{\n  \"defaultProvider\": \"google\"\n}\n"
  ) ;path-write-text
  (let ((vt (make-virtual-terminal :width 120 :height 24)))
    (virtual-terminal-push-key! vt " ")
    (virtual-terminal-push-key! vt 'q)
    (check (pi-config-run dir :terminal vt :env '()) => 'chat)
    (check (read-file dir "models/providers.json") => "[\"google\"]")
    (let ((screen (string-join (virtual-terminal-screen vt) "\n")))
      (check (string-contains screen "at least one provider must stay enabled") => #t)
    ) ;let
  ) ;let
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

;; 11. 回归：最后一个 provider 的模型列可完整向下导航。
;;     修复前 %provider-at 把 provider 下标错夹到倒数第二个 provider，
;;     m-count 错用其模型数（夹具中 minimax-cn 为 4），光标卡在索引 3
;;     （zai-coding-cn 的 glm-5-turbo），无法继续下移。
(let* ((dir (string-append FIXTURE "/config-tui"))
       (st (pi-config-state dir :env '()))
       (providers (pi-config-state-providers st))
       (n (length providers))
       (last-p (list-ref providers (- n 1)))
       (prev-p (list-ref providers (- n 2)))
      ) ;
  ;; 夹具形状：最后一个 provider 是 zai-coding-cn，模型数多于倒数第二个（minimax-cn）
  (check (pi-config-provider-id last-p) => "zai-coding-cn")
  (check (< (length (pi-config-provider-models prev-p))
           (length (pi-config-provider-models last-p))
         ) ;<
    =>
    #t
  ) ;check
  ;; 模型列 ↓ 越过倒数第二个 provider 的模型数边界（3 → 4）
  (let ((r (pi-config-key-handler st (pi-config-cursor (- n 1) 3) 1 'down)))
    (check (cdr (assoc 'model (car r))) => 4)
    (check (cdr (assoc 'provider (car r))) => (- n 1))
  ) ;let
  ;; G 直达最后一个 provider 的最后一个模型
  (let ((r (pi-config-key-handler st (pi-config-cursor (- n 1) 0) 1 "G")))
    (check (cdr (assoc 'model (car r)))
      =>
      (- (length (pi-config-provider-models last-p)) 1)
    ) ;check
  ) ;let
) ;let*

;; 11b. 端到端：导航到最后一个 provider 的模型列，↓ 连续下移不卡在索引 3。
;;      夹具 zai 列表：glm-4.6v(0)…glm-5.1(3) glm-5.2(4) glm-5.2-highspeed(5)…
;;      修复前光标卡在索引 3（preview 停在 glm-5.1）；修复后 5 次 ↓ 到索引 5。
(let ((dir (make-dir "last-provider-nav"))
      (vt (make-virtual-terminal :width 120 :height 24))
     ) ;
  ;; 5 次 ↓ 到最后一个 provider（zai-coding-cn），→ 进模型列，5 次 ↓
  (for-each (lambda (k) (virtual-terminal-push-key! vt k))
    (list 'down 'down 'down 'down 'down 'right 'down 'down 'down 'down 'down 'q)
  ) ;for-each
  (check (pi-config-run dir :terminal vt :env '()) => 'chat)
  (let ((screen (string-join (virtual-terminal-screen vt) "\n")))
    (check (string-contains screen "preview · glm-5.2-highspeed") => #t)
    (check (string-contains screen "preview · glm-5.1") => #f)
  ) ;let
) ;let

(check-report)
