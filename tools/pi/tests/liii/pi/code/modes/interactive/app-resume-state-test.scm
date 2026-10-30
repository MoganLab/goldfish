(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
  (liii pi code modes interactive thinking)
) ;import

(check-set-mode! 'report-failed)

;; 屏幕纯文本（去掉 ANSI 颜色码），便于断言提示文案
(define (screen-text app)
  (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
) ;define

;; footer 纯文本：会话名只应出现在 footer，聊天卡片里的历史文案不参与判断
(define (footer-text app)
  (string-join (map strip-ansi (component-render (%pi-app-footer app) 100)) "\n")
) ;define

;; 14. session_resumed 携带目标会话状态：按 :models 精确匹配后同步
;;     provider/model/thinking/footer，且只出现一张 Resume 卡片
(let* ((model-a (list (cons 'provider "acme") (cons 'id "big") (cons 'reasoning #t)))
       (model-b (list (cons 'provider "acme") (cons 'id "small") (cons 'reasoning #f)))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app
         (make-pi-app :terminal vterm
           :handler (lambda (a msg) #t)
           :provider "old"
           :model "old-model"
           :thinking-level "low"
           :supports-thinking #t
           :default-thinking-level "medium"
           :models (list model-a model-b)
         ) ;make-pi-app
       ) ;app
       (before (length (pi-app-chat app)))
       (state (list (cons 'thinkingLevel "high")
                 (cons 'model (list (cons 'provider "acme") (cons 'modelId "big") (cons 'reasoning #t)))
               ) ;list
       ) ;
      ) ;
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/x.jsonl" "新会话" state))
  (check (pi-app-provider app) => "acme")
  (check (pi-app-model app) => "big")
  (check (pi-app-thinking-level app) => "high")
  (check (pi-app-supports-thinking? app) => #t)
  (check (pi-app-available-thinking-levels app) => pi-thinking-levels)
  (check (length (pi-app-chat app)) => (+ before 1))
  (check-true (string-contains? (footer-text app) "big"))
  (check-true (string-contains? (footer-text app) "high"))
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "Resumed session: 新会话"))
  (check-true (string-contains? (screen-text app) "acme/big"))
  (check-false (string-contains? (screen-text app) "Default model"))
) ;let*

;; 15. 目标模型不在 :models 列表：保留当前 provider/model/thinking，
;;     只上屏一张警告卡片，不抛异常
(let* ((model-b (list (cons 'provider "acme") (cons 'id "small") (cons 'reasoning #f)))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app
         (make-pi-app :terminal vterm
           :handler (lambda (a msg) #t)
           :provider "old"
           :model "old-model"
           :thinking-level "low"
           :supports-thinking #t
           :models (list model-b)
         ) ;make-pi-app
       ) ;app
       (before (length (pi-app-chat app)))
       (state (list (cons 'thinkingLevel "high")
                 (cons 'model (list (cons 'provider "ghost") (cons 'modelId "gone") (cons 'reasoning #t)))
               ) ;list
       ) ;
      ) ;
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/x.jsonl" "旧模型会话" state))
  (check (pi-app-provider app) => "old")
  (check (pi-app-model app) => "old-model")
  (check (pi-app-thinking-level app) => "low")
  (check (length (pi-app-chat app)) => (+ before 1))
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "unavailable"))
  (check-true (string-contains? (screen-text app) "keeping current model"))
) ;let*

;; 16. 目标模型不支持思考：状态带 high 时收敛为 off 并提示
(let* ((model-b (list (cons 'provider "acme") (cons 'id "small") (cons 'reasoning #f)))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app
         (make-pi-app :terminal vterm
           :handler (lambda (a msg) #t)
           :provider "old"
           :model "old-model"
           :thinking-level "high"
           :supports-thinking #t
           :models (list model-b)
         ) ;make-pi-app
       ) ;app
       (state (list (cons 'thinkingLevel "high")
                 (cons 'model (list (cons 'provider "acme") (cons 'modelId "small")))
               ) ;list
       ) ;
      ) ;
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/x.jsonl" "无思考会话" state))
  (check (pi-app-model app) => "small")
  (check (pi-app-supports-thinking? app) => #f)
  (check (pi-app-thinking-level app) => "off")
  (check (pi-app-available-thinking-levels app) => '("off"))
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "unsupported; using \"off\""))
) ;let*

;; 17. 状态思考等级非法：收敛为 off 并提示
(let* ((model-a (list (cons 'provider "acme") (cons 'id "big") (cons 'reasoning #t)))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app
         (make-pi-app :terminal vterm
           :handler (lambda (a msg) #t)
           :provider "old"
           :model "old-model"
           :thinking-level "low"
           :supports-thinking #t
           :models (list model-a)
         ) ;make-pi-app
       ) ;app
       (state (list (cons 'thinkingLevel "ultra")
                 (cons 'model (list (cons 'provider "acme") (cons 'modelId "big") (cons 'reasoning #t)))
               ) ;list
       ) ;
      ) ;
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/x.jsonl" "非法等级会话" state))
  (check (pi-app-model app) => "big")
  (check (pi-app-thinking-level app) => "off")
  (pi-app-render! app)
  (check-true (string-contains? (screen-text app) "unknown thinking level"))
) ;let*

;; 18. 事件不含状态（3 元素或第 4 元素为 #f）：保留当前界面值，不提示
(let* ((model-a (list (cons 'provider "acme") (cons 'id "big") (cons 'reasoning #t)))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app
         (make-pi-app :terminal vterm
           :handler (lambda (a msg) #t)
           :provider "old"
           :model "old-model"
           :thinking-level "low"
           :supports-thinking #t
           :models (list model-a)
         ) ;make-pi-app
       ) ;app
      ) ;
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/x.jsonl" "无状态会话"))
  (check (pi-app-provider app) => "old")
  (check (pi-app-model app) => "old-model")
  (check (pi-app-thinking-level app) => "low")
  (pi-app-handle-resume-event! app (list 'session_resumed "/tmp/y.jsonl" "空状态会话" #f))
  (check (pi-app-provider app) => "old")
  (check (pi-app-model app) => "old-model")
  (check (pi-app-thinking-level app) => "low")
  (pi-app-render! app)
  (check-false (string-contains? (screen-text app) "unavailable"))
  (check-false (string-contains? (screen-text app) "unknown thinking level"))
) ;let*

(check-report)
