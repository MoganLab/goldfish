;; pi-valid-extension-name?
;; 判断扩展名是否符合命名规范。
;;
;; 语法
;; ----
;; (pi-valid-extension-name? name)
;;
;; 参数
;; ----
;; name : any
;; 待校验的扩展名。
;;
;; 返回值
;; ----
;; boolean
;; 符合命名规范时返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
  (liii check)
  (liii pi code core extensions install)
) ;import

(check-true (pi-valid-extension-name? "my-extension"))
(check-true (pi-valid-extension-name? "pi_plugin"))
(check-true (pi-valid-extension-name? "plugin123"))
(check-true (pi-valid-extension-name? "A_b-1"))

;; 拒绝非法字符与路径遍历注入
(check-false (pi-valid-extension-name? ""))
(check-false (pi-valid-extension-name? ".."))
(check-false (pi-valid-extension-name? "../foo"))
(check-false (pi-valid-extension-name? "foo/bar"))
(check-false (pi-valid-extension-name? "foo\\bar"))
(check-false (pi-valid-extension-name? "foo bar"))
(check-false (pi-valid-extension-name? "foo;rm -rf"))
(check-false (pi-valid-extension-name? 123))
(check-false (pi-valid-extension-name? #f))

(check-report)
