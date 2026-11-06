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

(define-library (liii subprocess)
  (export run run-either run-set! run-get run-allow! run-ban! run-unban! run-and
    run-or run-sequence run-pipe run-if run-when process? process-pid
    process-spawn process-alive? process-poll process-wait process-terminate
    process-kill process-kill-tree process-close process-close-stdin
    process-write process-write-stdin process-read process-read-line
    run-streaming run-streaming-chunks process-last-activity-time
    process-idle-seconds make-process-error process-error? process-error-code
    process-error-stderr process-error-stdout process-error-command
    process-error-message
  ) ;export
  (import (liii subprocess process-error)
    (liii subprocess registry)
    (liii subprocess process)
    (liii subprocess run)
    (liii subprocess combinator)
    (liii subprocess streaming)
  ) ;import
) ;define-library
