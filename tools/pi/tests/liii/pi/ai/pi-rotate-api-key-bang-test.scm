(import (liii check) (liii pi ai keyring))

(check-set-mode! 'report-failed)

(check (pi-rotate-api-key! (list (cons 'api-key "k")) "k" 429) => #f)

;; 429/403 委托 rotate-key 钩子换 key
(let ((rotated '()))
  (let ((cfg
          (list (cons 'api-key "k1")
            (cons 'rotate-key
              (lambda (config current-key status)
                (set! rotated (list current-key status))
                "k2"
              ) ;lambda
            ) ;cons
          ) ;list
        ) ;cfg
       ) ;
    (check (pi-rotate-api-key! cfg "k1" 429) => "k2")
    (check rotated => '("k1" 429))
  ) ;let
) ;let

(check-report)
