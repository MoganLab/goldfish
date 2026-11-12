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
  (liii tui text-width)
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

;; 切换 thinking 等级后，页脚立即用用户所选等级占位，
;; 下一次请求回报 active_node 时替换为节点收敛后的实际值
(let* ((f (make-pi-footer
            :model             "coding-fast"
            :provider          "all-in-one"
            :thinking-level    "low"
            :supports-thinking #t
            :active-node       '("google" "gemini-3.8-flash" "low" 1048576 #t)
          ) ;make-pi-footer
       ) ;f
       (render (lambda () (apply string-append (map strip-ansi (component-render f 200)))))
      ) ;
  (check (string-contains? (render) "-> (google) gemini-3.8-flash • low") => #t)
  (pi-footer-set-thinking-level! f "high")
  ;; 切换后立即显示用户所选等级，不再停留在旧值
  (check (string-contains? (render) "-> (google) gemini-3.8-flash • high") => #t)
  (check-false (string-contains? (render) "• low"))
  ;; 新请求回报收敛后的实际等级（该节点最高只支持 high）
  (pi-footer-set-active-node! f '("google" "gemini-3.8-flash" "high" 1048576 #t))
  (check (string-contains? (render) "-> (google) gemini-3.8-flash • high") => #t)
) ;let*

;; 尚无实际节点时切换等级：页脚保持 all-in-one/<chain-id>，
;; 不凭空编造节点后缀，等首次请求回报后再显示
(let* ((f (make-pi-footer
            :model             "coding-fast"
            :provider          "all-in-one"
            :thinking-level    "low"
            :supports-thinking #t
          ) ;make-pi-footer
       ) ;f
       (render (lambda () (apply string-append (map strip-ansi (component-render f 200)))))
      ) ;
  (pi-footer-set-thinking-level! f "high")
  (check (string-contains? (render) "all-in-one/coding-fast") => #t)
  (check-false (string-contains? (render) "->"))
  (pi-footer-set-active-node! f '("google" "gemini-3.8-flash" "high" 1048576 #t))
  (check (string-contains? (render) "-> (google) gemini-3.8-flash • high") => #t)
) ;let*

(check-report)
