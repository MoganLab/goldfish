(import (liii check) (liii pi ai keyring))

(check-set-mode! 'report-failed)

(check-true
  (not (pi-key-rotation? (list (cons 'api-key "k"))))
) ;check-true

(let ((cfg
        (list (cons 'api-key "k1")
          (cons 'rotate-key (lambda (config current-key status) "k2"))
        ) ;list
      ) ;cfg
     ) ;
  (check-true (pi-key-rotation? cfg))
) ;let

(check-true
  (not (pi-key-rotation? (list (cons 'rotate-key "nope"))))
) ;check-true

(check-report)
