(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

;; pi-footer-branch / pi-footer-set-branch!：读取与动态更新左下角分支显示
(let* ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"))
       (lines0 (component-render ft 60)))
  (check (pi-footer-branch ft) => "main")
  (check-true (string-contains? (list-ref lines0 0) "/tmp/test (main)"))
  (pi-footer-set-branch! ft "feature-x")
  (check (pi-footer-branch ft) => "feature-x")
  (let ((lines1 (component-render ft 60)))
    (check-true (string-contains? (list-ref lines1 0) "/tmp/test (feature-x)"))
    (check-false (string-contains? (list-ref lines1 0) "(main)"))))

;; set-branch! 不影响 footer 其他状态（model/thinking-level 等保持）
(let ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"
            :provider "kimi-coding" :model "kimi-for-coding"
            :thinking-level "medium" :supports-thinking #t)))
  (pi-footer-set-branch! ft "dev")
  (pi-footer-set-thinking-level! ft "high")
  (pi-footer-set-branch! ft "dev2")
  (let ((lines (component-render ft 100)))
    (check-true (string-contains? (list-ref lines 0) "/tmp/test (dev2)"))
    (check-true (string-contains? (list-ref lines 1) "(kimi-coding) kimi-for-coding • high"))))

;; 分支置 #f 时退化为仅显示路径（离开 git 仓库的场景）
(let ((ft (make-pi-footer :cwd "/tmp/plain" :branch "main")))
  (pi-footer-set-branch! ft #f)
  (let ((line1 (list-ref (component-render ft 60) 0)))
    (check-true (string-contains? line1 "/tmp/plain"))
    (check-false (string-contains? line1 "("))))

(check-report)
