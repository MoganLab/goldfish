;; pi-footer-set-active-node!
;; 记录 all-in-one 伪模型当前实际执行节点，供页脚右端追加显示。
;;
;; 语法
;; ----
;; (pi-footer-set-active-node! comp node)
;;
;; 参数
;; ----
;; comp : pi-footer?
;; 页脚组件。
;; node : (or list #f)
;; (provider model thinking-level) 三元组；#f 表示非链式模型。
;;
;; 返回值
;; ----
;; unspecified
;; 仅更新页脚状态，下一次渲染生效。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. 设置后右端追加 "-> (provider) model • level"
(let* ((ft (make-pi-footer
             :cwd            "/tmp/test"
             :provider       "all-in-one"
             :model          "coding-fast-cheap"
             :thinking-level "low"
           ) ;make-pi-footer
       ) ;ft
      ) ;
  (pi-footer-set-active-node! ft '("google" "gemini-3.8-flash" "medium"))
  (let ((line (strip-ansi (list-ref (component-render ft 200) 1))))
    (check-true (string-contains? line "coding-fast-cheap"))
    (check-true (string-contains? line "-> (google) gemini-3.8-flash • medium"))
  ) ;let
) ;let*

;; 2. 缺供应商时只显示 model 与级别
(let ((ft (make-pi-footer :cwd "/tmp/test" :provider "all-in-one" :model "chain")))
  (pi-footer-set-active-node! ft '(#f "model-only" "high"))
  (check
    (string-contains? (strip-ansi (list-ref (component-render ft 200) 1))
      "-> model-only • high"
    ) ;string-contains?
    =>
    #t
  ) ;check
) ;let

;; 3. 缺模型或显式清空时不追加后缀
(let ((ft (make-pi-footer :cwd "/tmp/test" :provider "all-in-one" :model "chain")))
  (pi-footer-set-active-node! ft '("p" #f "high"))
  (check
    (string-contains? (strip-ansi (list-ref (component-render ft 200) 1)) "->")
    =>
    #f
  ) ;check
  (pi-footer-set-active-node! ft #f)
  (check
    (string-contains? (strip-ansi (list-ref (component-render ft 200) 1)) "->")
    =>
    #f
  ) ;check
) ;let

;; 4. 窄终端截断后仍保持两行结构
(let* ((ft (make-pi-footer :cwd "/tmp/test" :provider "all-in-one" :model "chain"))
       (_ (pi-footer-set-active-node! ft '("google" "gemini-3.8-flash" "medium")))
       (lines (component-render ft 40))
      ) ;
  (check (length lines) => 2)
  (check-true
    (> (string-length (strip-ansi (list-ref lines 1))) 0)
  ) ;check-true
) ;let*

;; 5. 传入包含实际节点容量的 4 元组时同步更新页脚 context-window 与左侧上下文信息
(let ((ft (make-pi-footer :cwd "/tmp/test" :provider "all-in-one" :model "chain" :context-window 128000)))
  (check (pi-footer-context-window ft) => 128000)
  (pi-footer-set-active-node! ft '("google" "gemini-3.8-flash" "medium" 1048576))
  (check (pi-footer-context-window ft) => 1048576)
  (let ((line1 (strip-ansi (list-ref (component-render ft 100) 1))))
    (check-true (string-contains? line1 "1.0M"))
  ) ;let
) ;let

(check-report)
