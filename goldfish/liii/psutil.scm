;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
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

(define-library (liii psutil)
  (export process-memory-info process-num-threads memory-info?
    memory-info-resident-set-memory-size memory-info-virtual-memory-size
    memory-info-proportional-set-memory-size
  ) ;export
  (import (liii base) (liii os))
  (begin

    (define-record-type <memory-info>
      (%make-memory-info resident-set-memory-size
        virtual-memory-size
        proportional-set-memory-size
      ) ;%make-memory-info
      memory-info?
      (resident-set-memory-size memory-info-resident-set-memory-size)
      (virtual-memory-size memory-info-virtual-memory-size)
      (proportional-set-memory-size memory-info-proportional-set-memory-size)
    ) ;define-record-type

    (define* (process-memory-info (pid (getpid)))
      (let ((info (g_process-memory-info pid)))
        (%make-memory-info (car info) (cadr info) (caddr info))
      ) ;let
    ) ;define*

    (define* (process-num-threads (pid (getpid))) (g_process-num-threads pid))

  ) ;begin
) ;define-library
