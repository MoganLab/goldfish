;; pi-footer-branch
;; 获取状态栏当前显示的 Git 分支名称。
;;
;; 语法
;; ----
;; (pi-footer-branch comp)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏组件实例。
;;
;; 返回值
;; ----
;; string? or boolean?
;; 当前分支名；无分支时返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive footer))

(check-set-mode! 'report-failed)

;; 1. 获取构造时指定的分支
(let ((ft (make-pi-footer :branch "feature/123")))
  (check (pi-footer-branch ft) => "feature/123")
) ;let

;; 2. 默认或无分支
(let ((ft (make-pi-footer)))
  (check (pi-footer-branch ft) => #f)
) ;let

;; 3. set-branch 后获取新分支
(let ((ft (make-pi-footer)))
  (pi-footer-set-branch! ft "main")
  (check (pi-footer-branch ft) => "main")
) ;let

(check-report)
