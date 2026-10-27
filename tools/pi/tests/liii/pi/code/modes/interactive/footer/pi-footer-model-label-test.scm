(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

;; 模型显示串组装（对齐上游 pi）：
;; 支持 reasoning 时 off 也显式显示 " • thinking off"；
;; 不支持 reasoning 时只显示 model-id
(check (pi-footer-model-label "gemini-3.8-flash" "medium" #t) => "gemini-3.8-flash • medium")
(check (pi-footer-model-label "gemini-3.8-flash" "off" #t) => "gemini-3.8-flash • thinking off")
(check (pi-footer-model-label "kimi-for-coding" #f #f) => "kimi-for-coding")
(check (pi-footer-model-label "glm-5.3" "" #f) => "glm-5.3")
(check (pi-footer-model-label "glm-5.3" " high " #t) => "glm-5.3 • high")

;; 来自原 pi-app-thinking-test.scm 的 footer 标签断言。
;; 与上面相同的 medium/off/kimi/high 断言合并去重，仅保留新增覆盖。
(check (pi-footer-model-label "glm-5.3" "" #t) => "glm-5.3 • thinking off")

(let* ((f (make-pi-footer :model "gemini-3.8-flash"
            :thinking-level "off" :supports-thinking #t))
       (render (lambda () (string-join (component-render f 80) "\n"))))
  (check (string-contains? (render) "gemini-3.8-flash") => #t)
  (check (string-contains? (render) "thinking off") => #t)
  (pi-footer-set-thinking-level! f "high")
  (check (string-contains? (render) " • high") => #t)
  (pi-footer-set-thinking-level! f "off")
  (check (string-contains? (render) "thinking off") => #t))

;; 不支持 reasoning 的模型：右下角只显示 model-id，无 thinking 后缀
(let* ((f (make-pi-footer :model "gpt-5.2-flash"
            :thinking-level "off" :supports-thinking #f))
       (render (lambda () (string-join (component-render f 80) "\n"))))
  (check (string-contains? (render) " • ") => #f))

(check-report)
