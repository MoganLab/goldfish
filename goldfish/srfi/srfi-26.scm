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

(define-library (srfi srfi-26)
  (export cut cute)
  (begin

    ;; Reference implementation of SRFI-26 using syntax-rules
    ;; Based on the SRFI-26 reference implementation by Sebastian Egner

    ;; Internal helper: cut with accumulated slots
    (define-syntax %cut-internal
      (syntax-rules (<> <...>)
        ;; no more arguments - generate lambda
        ((_ (slot-name ...) (proc arg ...))
         (lambda (slot-name ...) (proc arg ...)))
        ;; <...> at end - generate lambda with rest arg
        ((_ (slot-name ...) (proc arg ...) <...>)
         (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
        ;; <> slot - add a new slot variable
        ((_ (slot-name ...) (proc arg ...) <> . remaining)
         (%cut-internal (slot-name ... x) (proc arg ... x) . remaining))
        ;; non-slot argument
        ((_ (slot-name ...) (proc arg ...) val . remaining)
         (%cut-internal (slot-name ...) (proc arg ... val) . remaining))))

    (define-syntax cut
      (syntax-rules ()
        ((_ . args)
         (%cut-internal () () . args))))

    ;; Internal helper: cute with accumulated slots and evaluations
    (define-syntax %cute-internal
      (syntax-rules (<> <...>)
        ;; no more arguments - generate let + lambda
        ((_ (slot-name ...) (eval-bind ...) (proc arg ...))
         (let (eval-bind ...) (lambda (slot-name ...) (proc arg ...))))
        ;; <...> at end
        ((_ (slot-name ...) (eval-bind ...) (proc arg ...) <...>)
         (let (eval-bind ...) (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot))))
        ;; <> slot
        ((_ (slot-name ...) (eval-bind ...) (proc arg ...) <> . remaining)
         (%cute-internal (slot-name ... x) (eval-bind ...) (proc arg ... x) . remaining))
        ;; non-slot - evaluate and bind
        ((_ (slot-name ...) (eval-bind ...) (proc arg ...) val . remaining)
         (%cute-internal (slot-name ...) (eval-bind ... (t val)) (proc arg ... t) . remaining))))

    (define-syntax cute
      (syntax-rules ()
        ((_ . args)
         (%cute-internal () () () . args))))

    ) ; end of begin
  ) ; end of library
