(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii tui)
  (liii go)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-model))

(check-set-mode! 'report-failed)

;; 屏幕纯文本（去掉 ANSI 颜色码），便于断言警告卡片

(define (screen-text app)
  (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
) ;define

(define %test-models
  (list (list (cons 'provider "google") (cons 'id "gemini-2.5-pro")
         (cons 'name "Gemini 2.5 Pro") (cons 'reasoning #t))
    (list (cons 'provider "zai") (cons 'id "glm-5")
      (cons 'name "GLM 5") (cons 'reasoning #t))
    (list (cons 'provider "google") (cons 'id "text-bison-99")
      (cons 'name "Fake No Reasoning") (cons 'reasoning #f))))

(define (make-test-app)
  (make-pi-app :terminal (make-virtual-terminal :width 100 :height 40)
    :handler (lambda (a msg) #t)
    :provider "google"
    :model "gemini-2.5-pro"
    :models %test-models
    :thinking-level "medium"
    :default-thinking-level "high")
) ;define

;; 1. /model 精确引用切换成功后投递一条 model_change，字段为切换后的 provider/model
(let ((app (make-test-app)))
  (pi-app-submit! app "/model glm-5")
  (check (pi-app-model app) => "glm-5")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "model_change" (("provider" . "zai") ("model-id" . "glm-5")))
  ) ;check
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 2. 切换为同一模型不投递状态任务
(let ((app (make-test-app)))
  (pi-app-submit! app "/model glm-5")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "model_change" (("provider" . "zai") ("model-id" . "glm-5")))
  ) ;check
  (pi-app-submit! app "/model glm-5")
  (check (pi-app-model app) => "glm-5")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 3. 选择器确认切换与精确引用走同一出口，同样只投递一次
(let ((app (make-test-app)))
  (pi-app-submit! app "/model")
  (pi-app-handle-input! app "glm")
  (pi-app-handle-input! app 'enter)
  (check (pi-app-model app) => "glm-5")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "model_change" (("provider" . "zai") ("model-id" . "glm-5")))
  ) ;check
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 4. 切换到不支持 reasoning 的模型只记录 model_change：
;;    thinking 收敛为 off 属于模型切换的联动，不单独记录 thinking_level_change
(let ((app (make-test-app)))
  (pi-app-submit! app "/model text-bison-99")
  (check (pi-app-thinking-level app) => "off")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "model_change"
       (("provider" . "google") ("model-id" . "text-bison-99")))
  ) ;check
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 5. 落盘失败只以警告卡片上屏，已切换的模型状态保持不变
(let ((app (make-test-app)))
  (pi-app-submit! app "/model glm-5")
  (chan-try-recv! (pi-app-jobs-chan app) #f)
  (chan-send! (pi-app-event-chan app) (list 'session_state_error "disk full") 1000)
  (pi-app-poll-events! app)
  (check (pi-app-model app) => "glm-5")
  (check (pi-app-provider app) => "zai")
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "disk full"))
) ;let

(check-report)
