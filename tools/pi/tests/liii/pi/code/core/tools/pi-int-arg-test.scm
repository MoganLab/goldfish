(import (liii check) (liii pi code core tools read))

(check-set-mode! 'report-failed)

;; pi-int-arg
;; 数值参数归一化为整数：模型可能给出实数（如 180.0），
;; 而 list-take/list-drop 只接受整数。
(check (pi-int-arg 180) => 180)
(check (pi-int-arg 180.0) => 180)
(check (pi-int-arg 180.4) => 180)
(check (pi-int-arg 0) => 0)
(check (pi-int-arg #f) => #f)
(check (pi-int-arg "180") => #f)
(check (pi-int-arg 'x) => #f)

(check-report)
