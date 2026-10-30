(import (scheme base)
  (liii check)
  (liii tui)
  (liii go)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-thinking))

(check-set-mode! 'report-failed)

(define (make-test-app . kwargs)
  (apply make-pi-app
    (append
      (list :terminal (make-virtual-terminal :width 100 :height 40)
        :handler (lambda (a msg) #t)
        :provider "google"
        :model "gemini-2.5-pro"
        :thinking-level "medium"
        :default-thinking-level "medium")
      kwargs))
) ;define

;; 1. /thinking 切换成功后投递一条 thinking_level_change
(let ((app (make-test-app)))
  (pi-app-submit! app "/thinking high")
  (check (pi-app-thinking-level app) => "high")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "thinking_level_change" (("level" . "high")))
  ) ;check
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 2. 切换为同一等级不投递状态任务
(let ((app (make-test-app)))
  (pi-app-submit! app "/thinking medium")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
  (pi-app-submit! app "/thinking high")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "thinking_level_change" (("level" . "high")))
  ) ;check
  (pi-app-submit! app "/thinking high")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 3. off 与普通等级一样照常记录
(let ((app (make-test-app)))
  (pi-app-submit! app "/thinking high")
  (chan-try-recv! (pi-app-jobs-chan app) #f)
  (pi-app-submit! app "/thinking off")
  (check (pi-app-thinking-level app) => "off")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "thinking_level_change" (("level" . "off")))
  ) ;check
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 4. shift+tab 循环切换属于用户切换，同样经唯一出口记录
(let ((app (make-test-app)))
  (check (pi-app-cycle-thinking! app) => "high")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f)
    =>
    '(session_state "thinking_level_change" (("level" . "high")))
  ) ;check
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 5. 未知等级只提示错误，不改变状态、不投递
(let ((app (make-test-app)))
  (pi-app-submit! app "/thinking bogus")
  (check (pi-app-thinking-level app) => "medium")
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

;; 6. 模型不支持思考时无法切换，也不投递状态任务
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 100 :height 40)
             :handler (lambda (a msg) #t)
             :provider "google"
             :model "text-bison-99"
             :supports-thinking #f
             :thinking-level "off")))
  (check (pi-app-cycle-thinking! app) => #f)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f)
) ;let

(check-report)
