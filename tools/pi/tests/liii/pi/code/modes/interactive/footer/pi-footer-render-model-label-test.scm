;; pi-footer 渲染的模型标签
;; 通过页脚渲染验证模型标签：支持思考的模型显示等级，不支持的只显示 model-id。
;;
;; 语法
;; ----
;; (make-pi-footer ...) / (pi-footer-set-thinking-level! comp tl) /
;; (component-render comp width)
;;
;; 参数
;; ----
;; :model、:thinking-level、:supports-thinking
;; 页脚初始化关键字参数，用于设定待渲染的模型标签。
;; comp : pi-footer?
;; 页脚组件。
;; tl : string
;; 新的思考等级。
;; width : integer
;; 渲染宽度。
;;
;; 返回值
;; ----
;; list
;; 两行页脚渲染结果。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

(let* ((f (make-pi-footer
            :model             "gemini-3.8-flash"
            :thinking-level    "off"
            :supports-thinking #t
          ) ;make-pi-footer
       ) ;f
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
(let* ((f (make-pi-footer
            :model             "gpt-5.2-flash"
            :thinking-level    "off"
            :supports-thinking #f
          ) ;make-pi-footer
       ) ;f
       (render (lambda () (string-join (component-render f 80) "\n")))
      ) ;
  (check (string-contains? (render) " • ") => #f)
) ;let*

(check-report)
