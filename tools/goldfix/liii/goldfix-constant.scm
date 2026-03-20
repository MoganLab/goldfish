(define-library (liii goldfix-constant)
  (import (scheme base))

  ;; ---------- 导出接口 ----------
  (export LPAREN RPAREN)

  (begin
    ;; 左括号字符常量
    (define LPAREN #\()

    ;; 右括号字符常量
    (define RPAREN #\))

  ) ;begin
) ;define-library
