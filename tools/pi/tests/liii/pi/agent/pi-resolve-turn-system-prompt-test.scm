(import (liii check)
        (liii pi agent)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-turn-system-prompt
;; 字符串直接返回；过程以本回合工具调用为参数求值。
(check (pi-resolve-turn-system-prompt "base prompt" '()) => "base prompt")

(check (pi-resolve-turn-system-prompt
         (lambda (tool-calls) (string-append "base+" (number->string (length tool-calls))))
         '("a" "b")
       ) ;pi-resolve-turn-system-prompt
  => "base+2"
) ;check

(check-report)
