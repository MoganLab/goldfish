(import (scheme base) (liii set) (native-scale timing))
(define xs (let loop ((i 9999) (xs '())) (if (< i 0) xs (loop (- i 1) (cons i xs)))))
(phase-measure 'profile-set (lambda () (let loop ((i 0)) (if (= i 5) 'PROFILE-OK (let ((s (list->set xs))) (unless (and (= (set-size s) 10000) (set-contains? s 9999) (not (set-contains? s 10000))) (error "profile set check failed")) (loop (+ i 1)))))))
