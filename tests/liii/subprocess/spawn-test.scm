(import (liii check) (liii subprocess) (liii string))

(check (process? 42) => #f)
(check (process? "not-a-proc") => #f)
(check (process? '()) => #f)

;; Test 1: Async spawn, poll, and wait
(let ((proc (process-spawn '("python3"
                             "-c"
                             "import sys, time; time.sleep(0.3); sys.exit(42)")
            ) ;process-spawn
      ) ;proc
     ) ;
  (check (process? proc) => #t)
  (check (> (process-pid proc) 0) => #t)
  (check (process-alive? proc) => #t)
  (check (process-poll proc) => #f)
  (let ((ret (process-wait proc 2000)))
    (check ret => 42)
    (check (process-alive? proc) => #f)
    (check (process-poll proc) => 42)
  ) ;let
  (process-close proc)
) ;let

;; Test 2: Timeout and terminate
(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(5)"))))
  (check (process? proc) => #t)
  (check (process-alive? proc) => #t)
  ;; Waiting with short timeout returns #f
  (let ((ret (process-wait proc 100)))
    (check ret => #f)
    (check (process-alive? proc) => #t)
  ) ;let
  ;; Terminate process
  (process-terminate proc)
  (let ((ret (process-wait proc 2000)))
    (check (integer? ret) => #t)
    (check (process-alive? proc) => #f)
  ) ;let
  (process-close proc)
) ;let

;; Test 3: Process kill
(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(5)"))))
  (check (process? proc) => #t)
  (check (process-alive? proc) => #t)
  (process-kill proc)
  (let ((ret (process-wait proc 2000)))
    (check (integer? ret) => #t)
    (check (process-alive? proc) => #f)
  ) ;let
  (process-close proc)
) ;let

;; Test 4: String representation
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (process-wait proc 2000)
  (let ((str (object->string proc)))
    (check (string-contains? str "#<process pid:") => #t)
    (check (string-contains? str "status:exited(0)") => #t)
  ) ;let
  (process-close proc)
) ;let

;; Test 5: Type error checking
(check-catch 'type-error (process-pid "invalid"))
(check-catch 'type-error (process-alive? 123))
(check-catch 'type-error (process-poll #f))
(check-catch 'type-error (process-wait '()))
(check-catch 'type-error (process-terminate "proc"))
(check-catch 'type-error (process-kill 0))
(check-catch 'type-error (process-close #t))

(check-report)
