(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code tui)
  (liii pi code tui thinking)
  (liii pi code tui worker)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: pi-next-thinking-level 按 7 级全集循环，非法输入容错回 "off"
;; -----------------------------------------------------------------------------
(check (pi-next-thinking-level "off") => "minimal")
(check (pi-next-thinking-level "minimal") => "low")
(check (pi-next-thinking-level "high") => "xhigh")
(check (pi-next-thinking-level "max") => "off")
(check (pi-next-thinking-level "bogus") => "off")

;; -----------------------------------------------------------------------------
;; 测试项 2: thinking 边框颜色映射（对齐上游 pi dark 主题实测 RGB）
;; -----------------------------------------------------------------------------
(check (pi-thinking-border-color "off") => "\x1b;[38;2;102;111;116m")
(check (pi-thinking-border-color "minimal") => "\x1b;[38;2;97;120;133m")
(check (pi-thinking-border-color "low") => "\x1b;[38;2;78;130;156m")
(check (pi-thinking-border-color "medium") => "\x1b;[38;2;91;127;200m")
(check (pi-thinking-border-color "high") => "\x1b;[38;2;151;118;229m")
(check (pi-thinking-border-color "xhigh") => "\x1b;[38;2;219;72;187m")
(check (pi-thinking-border-color "max") => "\x1b;[38;2;254;65;87m")
(check (pi-thinking-border-color "bogus") => "\x1b;[38;2;102;111;116m")

;; -----------------------------------------------------------------------------
;; 测试项 3: cycle 切换 app 会话级 thinking level，同步 footer 标签与编辑器边框色
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm
              :thinking-level "off" :supports-thinking #t))
       (ed (pi-app-editor app))
      ) ;
  (check (pi-app-thinking-level app) => "off")
  (check (editor-border-color ed) => (pi-thinking-border-color "off"))
  (check (pi-app-cycle-thinking! app) => "minimal")
  (check (pi-app-thinking-level app) => "minimal")
  (check (editor-border-color ed) => (pi-thinking-border-color "minimal"))
  (pi-app-render! app)
  (check (string-contains?
           (string-join (screen-lines (pi-app-screen app)) "\n")
           "minimal") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: 模型不支持 thinking：cycle 返回 #f 且状态不变
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm
              :thinking-level "off" :supports-thinking #f))
      ) ;
  (check (pi-app-cycle-thinking! app) => #f)
  (check (pi-app-thinking-level app) => "off")
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 5: footer 标签（对齐上游：支持 reasoning 时 off 显式显示 thinking off）
;; -----------------------------------------------------------------------------
(check (pi-footer-model-label "gemini-3.8-flash" "off" #t)
  => "gemini-3.8-flash • thinking off"
) ;check
(check (pi-footer-model-label "gemini-3.8-flash" "medium" #t)
  => "gemini-3.8-flash • medium"
) ;check
(check (pi-footer-model-label "kimi-for-coding" #f #f) => "kimi-for-coding")
(check (pi-footer-model-label "glm-5.3" "" #t) => "glm-5.3 • thinking off")
(check (pi-footer-model-label "glm-5.3" " high " #t) => "glm-5.3 • high")

(let* ((f (make-pi-footer :model "gemini-3.8-flash"
            :thinking-level "off" :supports-thinking #t))
       (render (lambda () (string-join (component-render f 80) "\n")))
      ) ;
  (check (string-contains? (render) "gemini-3.8-flash") => #t)
  (check (string-contains? (render) "thinking off") => #t)
  (pi-footer-set-thinking-level! f "high")
  (check (string-contains? (render) " • high") => #t)
  (pi-footer-set-thinking-level! f "off")
  (check (string-contains? (render) "thinking off") => #t)
) ;let*

;; 不支持 reasoning 的模型：右下角只显示 model-id，无 thinking 后缀
(let* ((f (make-pi-footer :model "gpt-5.2-flash"
            :thinking-level "off" :supports-thinking #f))
       (render (lambda () (string-join (component-render f 80) "\n")))
      ) ;
  (check (string-contains? (render) " • ") => #f)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 6: pi-cfg-with-thinking：覆盖已有项 / cfg 无该项时前置追加
;; -----------------------------------------------------------------------------
(check (pi-cfg-with-thinking '((thinking-level . "off") (model-id . "m")) "high")
  => '((thinking-level . "high") (model-id . "m"))
) ;check
(check (pi-cfg-with-thinking '((model-id . "m")) "low")
  => '((thinking-level . "low") (model-id . "m"))
) ;check

;; -----------------------------------------------------------------------------
;; 测试项 7: 同步 handler 路径：runner 收到的 cfg 已被 app 当前 level 覆盖
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (received-cfg #f)
       (handler
         (make-tui-agent-handler
           (list (cons 'help #f))
           '((provider . "google") (api-key . "k")
             (thinking-level . "off") (model-id . "m"))
           (lambda (parsed cfg text emit finish)
             (set! received-cfg cfg)
             '())
         ) ;make-tui-agent-handler
       ) ;handler
       (app (make-pi-app :terminal vterm :handler handler
              :thinking-level "high" :supports-thinking #t))
      ) ;
  (pi-app-submit! app "hi")
  (check (cdr (assoc 'thinking-level received-cfg)) => "high")
) ;let*

(check-report)
