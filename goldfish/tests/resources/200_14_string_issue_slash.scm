;; 测试字符串中斜杠符号导致的括号检查问题
;; 确保能复现 base64.scm 的实际问题

(define-constant BASE64_CHARS "/+")