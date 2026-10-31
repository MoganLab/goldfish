;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话级 thinking level 纯函数设施：
;; 级别全集与循环切换对齐上游 pi 的 app.thinking.cycle（shift+tab），
;; 边框颜色对齐上游 pi dark 主题的 thinking 边框色实测值，
;; 编辑器边框与 Working spinner 共用同一颜色，随级别联动
(define-library (liii pi code modes interactive thinking)
  (export pi-thinking-levels pi-next-thinking-level pi-thinking-border-color)
  (import (scheme base) (liii string) (liii list) (liii pi ai thinking))
  (begin

    ;; 级别全集与 (liii pi ai thinking) 共用同一来源（对齐上游 EXTENDED_THINKING_LEVELS）

    ;; 循环切换到下一个级别；非法输入容错回 "off"
    (define* (pi-next-thinking-level level (levels pi-thinking-levels))
      (if (null? levels)
        "off"
        (let ((idx (list-index (lambda (item) (equal? item level)) levels)))
          (if idx (list-ref levels (modulo (+ idx 1) (length levels))) "off")
        ) ;let
      ) ;if
    ) ;define*

    ;; 各级别的边框/ spinner 颜色（24-bit RGB 前缀，dark 主题实测）：
    ;; off 灰 → minimal 灰蓝 → low 蓝 → medium 长春花蓝 → high 紫 → xhigh 品红 → max 红
    (define %thinking-colors
      '(("off" . "\x1b;[38;2;102;111;116m")
        ("minimal" . "\x1b;[38;2;97;120;133m")
        ("low" . "\x1b;[38;2;78;130;156m")
        ("medium" . "\x1b;[38;2;91;127;200m")
        ("high" . "\x1b;[38;2;151;118;229m")
        ("xhigh" . "\x1b;[38;2;219;72;187m")
        ("max" . "\x1b;[38;2;254;65;87m"))
    ) ;define

    (define (pi-thinking-border-color level)
      (let ((pair (assoc level %thinking-colors)))
        (if pair (cdr pair) (cdr (assoc "off" %thinking-colors)))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
