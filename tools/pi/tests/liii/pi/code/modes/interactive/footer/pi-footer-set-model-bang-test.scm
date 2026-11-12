;; pi-footer-set-model!
;; 执行 pi-footer-set-model! 操作并更新状态。
;;
;; 语法
;; ----
;; (pi-footer-set-model! ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report)

;; 切换 provider/model/supports 后右下角标签联动
(let ((ft (make-pi-footer
            :cwd               "/tmp/test"
            :branch            "main"
            :provider          "google"
            :model             "gemini-2.5-pro"
            :thinking-level    "high"
            :supports-thinking #t
          ) ;make-pi-footer
      ) ;ft
     ) ;
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
) ;let

;; 切换模型时清除已有的 active-node，并可选同步 context-window
(let ((ft (make-pi-footer
            :cwd            "/tmp/test"
            :provider       "all-in-one"
            :model          "coding-fast"
            :context-window 1000000
          ) ;make-pi-footer
      ) ;ft
     ) ;
  (pi-footer-set-active-node! ft '("google" "gemini-3.8-flash" "medium" 1048576))
  (check-true (pair? (pi-footer-active-node ft)))
  ;; 切换至常规模型：active-node 必须被清空为 #f，右下角恢复常规无箭头展示
  (pi-footer-set-model! ft "deepseek" "deepseek-v4-pro" #t 1000000)
  (check (pi-footer-active-node ft) => #f)
  (let ((line (list-ref (component-render ft 100) 1)))
    (check-true (string-contains? line "(deepseek) deepseek-v4-pro"))
    (check-false (string-contains? line "->"))
  ) ;let
) ;let

(check-report)
