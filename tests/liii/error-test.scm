(import (liii check)
  (liii error)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


(check-catch 'os-error (os-error))


(check-catch 'file-not-found-error
  (file-not-found-error)
) ;check-catch


(check-catch 'not-a-directory-error
  (not-a-directory-error)
) ;check-catch


(check-catch 'file-exists-error
  (file-exists-error)
) ;check-catch


(check-catch 'timeout-error
  (timeout-error)
) ;check-catch


(check-catch 'type-error (type-error))
(check-catch 'type-error
  (type-error "msg")
) ;check-catch
(check-catch 'type-error
  (type-error "msg" "msg2")
) ;check-catch


(check-true (type-error? 'type-error))
(check-true (type-error? 'wrong-type-arg)
) ;check-true


(check-catch 'key-error (key-error))


(check-catch 'value-error (value-error))


(check-catch 'index-error (index-error))


(check-catch '??? (???))


(check-report)
