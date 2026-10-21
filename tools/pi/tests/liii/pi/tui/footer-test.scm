(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi tui footer)
) ;import

(check-set-mode! 'report)

;; 1. 基本 Footer 渲染
(let* ((ft (make-pi-footer
             :cwd "/home/da/git/goldfish"
             :home "/home/da"
             :branch "da/1082/pi_enter"
             :provider "google"
             :model "gemini-3.8-flash • medium"
             :stats "0.0%/1.0M (auto)"
           ) ;make-pi-footer
       ) ;ft
       (lines (component-render ft 80))
      ) ;let*
  (check (component? ft) => #t)
  (check (length lines) => 2)
  ;; 行 1：路径与分支
  (check-true (string-contains? (list-ref lines 0) "~/git/goldfish (da/1082/pi_enter)"))
  ;; 行 2：左下角统计与右下角模型
  (check-true (string-contains? (list-ref lines 1) "0.0%/1.0M (auto)"))
  (check-true (string-contains? (list-ref lines 1) "(google) gemini-3.8-flash • medium"))
  (check (<= (display-width (list-ref lines 0)) 80) => #t)
  (check (<= (display-width (list-ref lines 1)) 80) => #t)
) ;let*

;; 2. 动态更新分支与统计
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"))
       (lines (component-render ft 60))
      ) ;let*
  (check-true (string-contains? (list-ref lines 0) "/tmp/test (main)"))
) ;let*

;; 3. 主目录缩写
(check (format-cwd-for-footer "/home/da" "/home/da") => "~")
(check (format-cwd-for-footer "/home/da/git/goldfish" "/home/da") => "~/git/goldfish")
(check (format-cwd-for-footer "/tmp/test" "/home/da") => "/tmp/test")
(check (format-cwd-for-footer "/home/da" #f) => "/home/da")

(check-report)
