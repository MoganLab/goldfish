(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

;; 切换 provider/model/supports 后右下角标签联动
(let ((ft (make-pi-footer :cwd "/tmp/test" :branch "main"
            :provider "google" :model "gemini-2.5-pro"
            :thinking-level "high" :supports-thinking #t)))
  (pi-footer-set-model! ft "zai" "glm-5" #t)
  (let ((line (list-ref (component-render ft 100) 1)))
    (check-true (string-contains? line "(zai) glm-5 • high")))
  ;; 不支持 reasoning → 不显示 thinking 尾注
  (pi-footer-set-model! ft "google" "text-bison-99" #f)
  (let ((line (list-ref (component-render ft 100) 1)))
    (check-true (string-contains? line "(google) text-bison-99"))
    (check-false (string-contains? line "•"))))

(check-report)
