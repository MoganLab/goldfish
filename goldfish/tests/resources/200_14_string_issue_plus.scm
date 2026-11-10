;; 测试字符串中+/符号导致括号检查错误的用例
;; 这是base64.scm第26行的简化版本

(define-constant BYTE2BASE64_BV
  (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))