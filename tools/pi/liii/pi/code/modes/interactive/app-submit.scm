;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 编辑器输入变化：识别 bash 模式并联动边框颜色
(define-library (liii pi code modes interactive app-submit)
  (export %starts-with-char? %pi-app-editor-on-change)
  (import (scheme base)
    (liii string)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive bash)
  ) ;import
  (begin

    (define (%starts-with-char? s c)
      (and (> (string-length s) 0) (char=? (string-ref s 0) c))
    ) ;define

    ;; bash 模式边框联动（对齐 pi onChange + updateEditorBorderColor）：
    ;; 文本（去前导空白后）以 ! 开头 → 绿色；否则恢复当前 thinking 级别颜色
    (define (%pi-app-editor-on-change app ed text)
      (let ((bash? (%starts-with-char? (string-trim-left text) #\!)))
        (%pi-app-bash-mode?-set! app bash?)
        (editor-set-border-color! ed
          (if bash?
            pi-bash-border-color
            (pi-thinking-border-color (pi-app-thinking-level app))
          ) ;if
        ) ;editor-set-border-color!
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
