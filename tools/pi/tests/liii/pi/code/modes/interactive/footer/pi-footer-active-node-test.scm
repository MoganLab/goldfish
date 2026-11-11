;; pi-footer-active-node
;; 读取页脚记录的 all-in-one 当前实际执行节点。
;;
;; 语法
;; ----
;; (pi-footer-active-node comp)
;;
;; 参数
;; ----
;; comp : pi-footer?
;; 页脚组件。
;;
;; 返回值
;; ----
;; (or list #f)
;; (provider model thinking-level) 三元组；未设置时为 #f。

(import (scheme base) (liii check) (liii pi code modes interactive footer))

(check-set-mode! 'report-failed)

;; 1. 默认无当前节点
(check (pi-footer-active-node (make-pi-footer :cwd "/tmp")) => #f)

;; 2. 构造参数可直接指定
(check (pi-footer-active-node (make-pi-footer
                                :cwd "/tmp"
                                :active-node
                                '("google" "gemini-3.8-flash" "medium")
                              ) ;make-pi-footer
       ) ;pi-footer-active-node
  =>
  '("google" "gemini-3.8-flash" "medium")
) ;check

;; 3. setter 写入后读取一致
(let ((ft (make-pi-footer :cwd "/tmp")))
  (pi-footer-set-active-node! ft '("zai-coding-cn" "glm-5.3" "high"))
  (check (pi-footer-active-node ft) => '("zai-coding-cn" "glm-5.3" "high"))
  (pi-footer-set-active-node! ft #f)
  (check (pi-footer-active-node ft) => #f)
) ;let

(check-report)
