(import (scheme base)
  (liii check)
  (liii pi code extension install)
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
