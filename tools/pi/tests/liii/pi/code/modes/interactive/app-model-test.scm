(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-model)
  (liii pi code modes interactive model-selector)
) ;import

(check-set-mode! 'report-failed)

(define %test-models
  (list (list (cons 'provider "google") (cons 'id "gemini-2.5-pro")
         (cons 'name "Gemini 2.5 Pro") (cons 'reasoning #t))
    (list (cons 'provider "google") (cons 'id "gemini-3-flash-preview")
      (cons 'name "Gemini 3 Flash Preview") (cons 'reasoning #t))
    (list (cons 'provider "zai") (cons 'id "glm-5")
      (cons 'name "GLM 5") (cons 'reasoning #t))
    (list (cons 'provider "google") (cons 'id "text-bison-99")
      (cons 'name "Fake No Reasoning") (cons 'reasoning #f))
  ) ;list
) ;define

(define (make-test-app . kwargs)
  (apply make-pi-app
    (append
      (list :terminal (make-virtual-terminal :width 100 :height 40)
        :handler (lambda (a msg) #t)
        :provider "google"
        :model "gemini-2.5-pro"
        :models %test-models
        :thinking-level "high"
        :default-thinking-level "medium"
      ) ;list
      kwargs
    ) ;append
  ) ;apply
) ;define

;; 1. 命令识别：/model 与 /model <query>，排除 /models 等前缀误匹配。
(check-true (pi-app-model-command? "/model"))
(check-true (pi-app-model-command? "/model glm-5"))
(check-false (pi-app-model-command? "/models"))
(check-false (pi-app-model-command? "hello /model"))

;; 2. /model 打开模型选择器（编辑器失焦），Escape 取消恢复输入。
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model")
  (check-true (model-selector? (pi-app-active-selector app)))
  (check-false (editor-focused? (pi-app-editor app)))
  (pi-app-render! app)
  (let ((plain (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? plain "Model"))
    (check-true (string-contains? plain "✓ gemini-2.5-pro"))
  ) ;let
  (pi-app-handle-input! app 'escape)
  (check (pi-app-active-selector app) => #f)
  (check-true (editor-focused? (pi-app-editor app)))
) ;let*

;; 3. 选择器内搜索 + Enter 切换（跨 provider）。
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model")
  (pi-app-handle-input! app "glm")
  (pi-app-handle-input! app 'enter)
  (check (pi-app-model app) => "glm-5")
  (check (pi-app-provider app) => "zai")
  (check (pi-app-active-selector app) => #f)
) ;let*

;; 4. /model <id> 精确匹配直接切换：
;;    - 编辑器清空、footer 显示新 provider/model
;;    - 会话卡片提示 "Model: <id>"
;;    - job 携带 (msg thinking-level provider model-id)
;;    - thinking 回退到默认级别（对齐上游 _getThinkingLevelForModelSwitch）
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model glm-5")
  (check (pi-app-model app) => "glm-5")
  (check (pi-app-provider app) => "zai")
  (check (editor-text (pi-app-editor app)) => "")
  (check (pi-app-thinking-level app) => "medium")
  (pi-app-render! app)
  (let ((plain (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? plain "Model: glm-5"))
    (check-true (string-contains? plain "(zai) glm-5"))
  ) ;let
  (check (%pi-app-job-payload app "hi") => '("hi" "medium" "zai" "glm-5"))
) ;let*

;; 5. provider/id 复合引用与大小写不敏感精确匹配；前缀与模型不匹配时回退选择器。
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model zai/glm-5")
  (check (pi-app-model app) => "glm-5")
) ;let*
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model GLM-5")
  (check (pi-app-model app) => "glm-5")
) ;let*
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model google/glm-5")
  (check-true (model-selector? (pi-app-active-selector app)))
) ;let*

;; 6. /model <未知> 回退选择器并预填搜索词（对齐上游 showModelSelector(searchTerm)）。
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model unknown-model")
  (check-true (model-selector? (pi-app-active-selector app)))
  (check (editor-text (model-selector-search (pi-app-active-selector app))) => "unknown-model")
) ;let*

;; 7. 切换到不支持 reasoning 的模型：supports/可用级别/thinking 收敛为 off。
(let* ((app (make-test-app)))
  (check (pi-app-thinking-level app) => "high")
  (pi-app-submit! app "/model text-bison-99")
  (check (pi-app-supports-thinking? app) => #f)
  (check (pi-app-thinking-level app) => "off")
  (check (pi-app-available-thinking-levels app) => '("off"))
  (pi-app-render! app)
  (let ((plain (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? plain "text-bison-99"))
    (check-false (string-contains? plain "thinking off"))
  ) ;let
) ;let*

;; 8. Ctrl+S 选择并保存默认模型：触发 on-save-model 回调，卡片提示 Default model。
(define saved-model '())
(let* ((app (make-test-app
              :on-save-model (lambda (p m) (set! saved-model (cons (cons p m) saved-model))))))
  (pi-app-submit! app "/model")
  (pi-app-handle-input! app "glm")
  (pi-app-handle-input! app 'ctrl-s)
  (check saved-model => '(("zai" . "glm-5")))
  (check (pi-app-model app) => "glm-5")
  (check (pi-app-active-selector app) => #f)
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Default model: zai/glm-5"))
) ;let*

;; 9. 会话内 shift+tab 循环使用切换后的可用级别（非 reasoning 模型只剩 off）。
(let* ((app (make-test-app)))
  (pi-app-submit! app "/model text-bison-99")
  (check (pi-app-cycle-thinking! app) => #f)
) ;let*

(check-report)
