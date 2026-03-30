(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
内部校验 check-bag 的函数也要覆盖错误分支。
|#
(check-catch 'type-error (bag-member "not a bag" 1 #f))

(check-report)
