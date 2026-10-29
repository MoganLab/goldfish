;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views theme)
  (export %bg-user %fg-user %bg-pending %bg-success %bg-error %fg-title
    %fg-output %fg-accent %fg-warning %fg-reset %bold-reset %fg-dim %ansi-reset
    apply-card-bg
  ) ;export
  (import (scheme base) (liii tui text-width))
  (begin

    ;; pi dark 主题 dark.json 的 okhsl 精确换算值。
    (define %bg-user "\x1b;[48;2;33;59;73m")
    (define %fg-user "\x1b;[38;2;222;224;225m")
    (define %bg-pending "\x1b;[48;2;52;56;58m")
    (define %bg-success "\x1b;[48;2;37;65;49m")
    (define %bg-error "\x1b;[48;2;91;40;42m")
    (define %fg-title "\x1b;[1m\x1b;[38;2;222;224;225m")
    (define %fg-output "\x1b;[38;2;157;165;169m")
    (define %fg-accent "\x1b;[38;2;167;152;215m")
    (define %fg-warning "\x1b;[38;2;205;154;34m")

    ;; 只关闭前景/粗体，不清除行背景。
    (define %fg-reset "\x1b;[39m")
    (define %bold-reset "\x1b;[22m\x1b;[39m")
    (define %fg-dim "\x1b;[38;2;126;136;142m")
    (define %ansi-reset "\x1b;[0m")

    ;; 补足可见宽度，padding 也包含在卡片背景色内。
    (define (apply-card-bg line width bg-code fg-code)
      (let* ((vlen (display-width line))
             (pad-needed (max 0 (- width vlen)))
             (padded (string-append line (make-string pad-needed #\space)))
            ) ;
        (string-append bg-code fg-code padded %ansi-reset)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
