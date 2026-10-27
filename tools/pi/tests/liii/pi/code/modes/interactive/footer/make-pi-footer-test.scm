(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

;; 基本 Footer 渲染
(let* ((ft (make-pi-footer
             :cwd "/home/da/git/goldfish"
             :home "/home/da"
             :branch "da/1082/pi_enter"
             :provider "google"
             :model "gemini-3.8-flash"
             :thinking-level "medium"
             :supports-thinking #t
             :stats "0.0%/1.0M (auto)"))
       (lines (component-render ft 80)))
  (check (component? ft) => #t)
  (check (length lines) => 2)
  ;; 行 1：路径与分支
  (check-true (string-contains? (list-ref lines 0) "~/git/goldfish (da/1082/pi_enter)"))
  ;; 行 2：左下角统计与右下角模型
  (check-true (string-contains? (list-ref lines 1) "0.0%/1.0M (auto)"))
  (check-true (string-contains? (list-ref lines 1) "(google) gemini-3.8-flash • medium"))
  (check (<= (display-width (list-ref lines 0)) 80) => #t)
  (check (<= (display-width (list-ref lines 1)) 80) => #t))

;; 动态更新分支与统计
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"))
       (lines (component-render ft 60)))
  (check-true (string-contains? (list-ref lines 0) "/tmp/test (main)")))

;; 未显式指定 provider/model 时按默认渲染，不再显示假的 thinking 后缀
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"))
       (lines (component-render ft 80))
       (line2 (list-ref lines 1)))
  (check-true (string-contains? line2 "(google) gemini-3.8-flash"))
  (check-false (string-contains? line2 "•")))

;; 显式传入 #f 时回退默认值
(let* ((ft (make-pi-footer :provider #f :model #f))
       (lines (component-render ft 80)))
  (check-true (string-contains? (list-ref lines 1) "(google) gemini-3.8-flash")))

(check-report)
