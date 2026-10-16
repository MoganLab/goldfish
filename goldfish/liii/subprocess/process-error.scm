;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii subprocess process-error)
  (export <process-error> make-process-error process-error? process-error-code
    process-error-stderr process-error-stdout process-error-command
    process-error-message
  ) ;export
  (import (scheme base) (liii base) (liii error))
  (begin

    (define <process-error> '<process-error>)

    (define-record-type <process-error>
      (%make-process-error command code stdout stderr message)
      process-error?
      (command process-error-command)
      (code process-error-code)
      (stdout process-error-stdout)
      (stderr process-error-stderr)
      (message process-error-message)
    ) ;define-record-type

    (define (%default-error-message command code stdout stderr)
      (cond
       ((and (string? stderr) (not (string=? stderr ""))) stderr)
       ((and (string? stdout) (not (string=? stdout ""))) stdout)
       (else (format #f "Command '~a' exited with status ~a" command code))
      ) ;cond
    ) ;define

    (define (make-process-error arg1 arg2 arg3 arg4 . rest)
      (let* ((by-code? (integer? arg1))
             (code (if by-code? arg1 arg2))
             (command (if by-code? arg4 arg1))
             (stdout (if by-code? arg3 arg3))
             (stderr (if by-code? arg2 arg4))
             (msg (if (pair? rest) (car rest) (%default-error-message command code stdout stderr))
             ) ;msg
            ) ;
        (%make-process-error command code stdout stderr msg)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
