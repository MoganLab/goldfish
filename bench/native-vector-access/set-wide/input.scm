(import (scheme base) (liii set) (native-scale timing))
(define xs (let loop ((i 49999) (xs '())) (if (< i 0) xs (loop (- i 1) (cons i xs)))))
(phase-measure 'profile-set (lambda () (let loop ((i 0)) (if (= i 1) 'PROFILE-OK (let ((s (list->set xs))) (unless (and (= (set-size s) 50000) (set-contains? s 49999) (not (set-contains? s 50000))) (error "profile set check failed")) (loop (+ i 1)))))))
