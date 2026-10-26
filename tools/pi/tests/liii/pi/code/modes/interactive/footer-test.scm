(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report)

;; 1. 基本 Footer 渲染
(let* ((ft (make-pi-footer
             :cwd "/home/da/git/goldfish"
             :home "/home/da"
             :branch "da/1082/pi_enter"
             :provider "google"
             :model "gemini-3.8-flash"
             :thinking-level "medium"
             :supports-thinking #t
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

;; 3. 模型显示串组装（对齐上游 pi）：
;;    支持 reasoning 时 off 也显式显示 " • thinking off"；
;;    不支持 reasoning 时只显示 model-id
(check (pi-footer-model-label "gemini-3.8-flash" "medium" #t) => "gemini-3.8-flash • medium")
(check (pi-footer-model-label "gemini-3.8-flash" "off" #t) => "gemini-3.8-flash • thinking off")
(check (pi-footer-model-label "kimi-for-coding" #f #f) => "kimi-for-coding")
(check (pi-footer-model-label "glm-5.3" "" #f) => "glm-5.3")
(check (pi-footer-model-label "glm-5.3" " high " #t) => "glm-5.3 • high")

;; 4. 未显式指定 provider/model 时按默认渲染，不再显示假的 thinking 后缀
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"))
       (lines (component-render ft 80))
       (line2 (list-ref lines 1))
      ) ;let*
  (check-true (string-contains? line2 "(google) gemini-3.8-flash"))
  (check-false (string-contains? line2 "•"))
) ;let*

;; 5. 显式传入 #f 时回退默认值
(let* ((ft (make-pi-footer :provider #f :model #f))
       (lines (component-render ft 80))
      ) ;let*
  (check-true (string-contains? (list-ref lines 1) "(google) gemini-3.8-flash"))
) ;let*

;; 6. 主目录缩写
(check (format-cwd-for-footer "/home/da" "/home/da") => "~")
(check (format-cwd-for-footer "/home/da/git/goldfish" "/home/da") => "~/git/goldfish")
(check (format-cwd-for-footer "/tmp/test" "/home/da") => "/tmp/test")
(check (format-cwd-for-footer "/home/da" #f) => "/home/da")

;; 7. pi-footer-branch / pi-footer-set-branch!：读取与动态更新左下角分支显示
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"))
       (lines0 (component-render ft 60))
      ) ;let*
  (check (pi-footer-branch ft) => "main")
  (check-true (string-contains? (list-ref lines0 0) "/tmp/test (main)"))
  (pi-footer-set-branch! ft "feature-x")
  (check (pi-footer-branch ft) => "feature-x")
  (let ((lines1 (component-render ft 60)))
    (check-true (string-contains? (list-ref lines1 0) "/tmp/test (feature-x)"))
    (check-false (string-contains? (list-ref lines1 0) "(main)"))
  ) ;let
) ;let*

;; 8. set-branch! 不影响 footer 其他状态（model/thinking-level 等保持）
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"
             :provider "kimi-coding" :model "kimi-for-coding"
             :thinking-level "medium" :supports-thinking #t))
      ) ;let*
  (pi-footer-set-branch! ft "dev")
  (pi-footer-set-thinking-level! ft "high")
  (pi-footer-set-branch! ft "dev2")
  (let ((lines (component-render ft 100)))
    (check-true (string-contains? (list-ref lines 0) "/tmp/test (dev2)"))
    (check-true (string-contains? (list-ref lines 1) "(kimi-coding) kimi-for-coding • high"))
  ) ;let
) ;let*

;; 9. 分支置 #f 时退化为仅显示路径（离开 git 仓库的场景）
(let* ((ft (make-pi-footer :cwd "/tmp/plain" :branch "main")))
  (pi-footer-set-branch! ft #f)
  (let ((line1 (list-ref (component-render ft 60) 0)))
    (check-true (string-contains? line1 "/tmp/plain"))
    (check-false (string-contains? line1 "("))
  ) ;let
) ;let*

;; 10. 会话名称显示与动态更新，不影响分支和模型状态
(let* ((ft (make-pi-footer :cwd "/tmp/named" :branch "main"
             :provider "google" :model "gemini-3.8-flash"
             :session-name "Refactor auth module"))
       (line1 (list-ref (component-render ft 100) 0)))
  (check-true (string-contains? line1 "/tmp/named (main) • Refactor auth module"))
  (pi-footer-set-session-name! ft "Fix Windows build")
  (let ((updated (list-ref (component-render ft 100) 0)))
    (check-true (string-contains? updated "/tmp/named (main) • Fix Windows build"))
    (check-false (string-contains? updated "Refactor auth module")))
  (pi-footer-set-session-name! ft #f)
  (check-true (string-contains? (list-ref (component-render ft 100) 0) "/tmp/named (main)")))

;; 11. pi-footer-set-model!：切换 provider/model/supports 后右下角标签联动
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"
             :provider "google" :model "gemini-2.5-pro"
             :thinking-level "high" :supports-thinking #t))
      ) ;let*
  (pi-footer-set-model! ft "zai" "glm-5" #t)
  (let ((line (list-ref (component-render ft 100) 1)))
    (check-true (string-contains? line "(zai) glm-5 • high"))
  ) ;let
  ;; 不支持 reasoning → 不显示 thinking 尾注
  (pi-footer-set-model! ft "google" "text-bison-99" #f)
  (let ((line (list-ref (component-render ft 100) 1)))
    (check-true (string-contains? line "(google) text-bison-99"))
    (check-false (string-contains? line "•"))
  ) ;let
) ;let*

(check-report)
