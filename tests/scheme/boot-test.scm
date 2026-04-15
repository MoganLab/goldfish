;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii list)
        (liii check)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

(when (not (os-windows?))
  (check (file-exists? "/tmp") => #t)
  (check (file-exists? "/not_exists") => #f)
) ;when

(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-catch 'permission-error (file-exists? "/root"))
) ;when

(when (os-windows?)
  (check (file-exists? "C:") => #t)
) ;when

(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-catch 'permission-error (delete-file "/root"))
) ;when

(when (not (os-windows?))
  (with-output-to-file "/tmp/test_delete_file"
    (lambda ()
      (display "Hello, World!")
    ) ;lambda
  ) ;with-output-to-file
  (check (file-exists? "/tmp/test_delete_file") => #t)
  (delete-file "/tmp/test_delete_file")
  (check (file-exists? "/tmp/test_delete_file") => #f)
) ;when

(define (sum start end)
  (if (= start end)
      start
      (+ (sum start (- end 1)) end)
  ) ;if
) ;define

(check (sum 2 4) => 9)

(check-report "\n\nCheck report of boot-test.scm => ")
