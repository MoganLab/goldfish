(import (liii check))
(import (liii goldfmt scan normalize))

(check-set-mode! 'report-failed)

;; split-pair-list: 真列表
(call-with-values
  (lambda () (split-pair-list '(a b c)))
  (lambda (elems tail)
    (check elems => '(a b c))
    (check tail => '())))

;; split-pair-list: 点对尾
(call-with-values
  (lambda () (split-pair-list '(a b . c)))
  (lambda (elems tail)
    (check elems => '(a b))
    (check tail => 'c)))

;; split-pair-list: 空表
(call-with-values
  (lambda () (split-pair-list '()))
  (lambda (elems tail)
    (check elems => '())
    (check tail => '())))

(check-report)
