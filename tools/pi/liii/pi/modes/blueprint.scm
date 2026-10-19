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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi modes blueprint)
  (import (scheme base)
    (liii pi args)
    (liii pi modes blueprint common)
    (liii pi modes blueprint generate)
    (liii pi modes blueprint manage)
    (liii pi modes blueprint verify)
  ) ;import
  (export run-blueprint-flow pi-blueprint-extend-tool-names)
  (begin

    (define (pi-blueprint-extend-tool-names base parsed)
      (pi-bp-mode-extend-tools base parsed)
    ) ;define

    (define (run-blueprint-flow parsed cfg mode initial-msg)
      (cond ((pi-args-ref parsed 'blueprint)
             (run-blueprint-generate-flow parsed cfg mode initial-msg)
            ) ;
            ((pi-args-ref parsed 'plan-status) (run-blueprint-status-flow parsed mode))
            ((pi-args-ref parsed 'plan-next) (run-blueprint-next-flow parsed cfg mode))
            ((pi-args-ref parsed 'plan-verify) (run-blueprint-verify-flow parsed cfg mode))
            ((pi-args-ref parsed 'plan-abandon) (run-blueprint-abandon-flow parsed mode))
            (else 0)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
