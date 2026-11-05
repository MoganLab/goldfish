;; pi-key-rotation?
;; 判断当前配置是否启用了自动轮换 API 密钥功能。
;;
;; 语法
;; ----
;; (pi-key-rotation? config)
;;
;; 参数
;; ----
;; config : alist?
;; 请求配置关联表。
;;
;; 返回值
;; ----
;; boolean
;; 当配置中包含有效的 rotate-key 过程钩子时返回 #t，否则返回 #f。

(import (liii check) (liii pi ai keyring))

(check-set-mode! 'report-failed)

(check-true
  (not (pi-key-rotation? (list (cons 'api-key "k"))))
) ;check-true

(let ((cfg
        (list (cons 'api-key "k1")
          (cons 'rotate-key (lambda (config current-key status) "k2"))
        ) ;list
      ) ;cfg
     ) ;
  (check-true (pi-key-rotation? cfg))
) ;let

(check-true
  (not (pi-key-rotation? (list (cons 'rotate-key "nope"))))
) ;check-true

(check-report)
