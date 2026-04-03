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

(import (srfi srfi-78)
        (scheme process-context)
        (srfi srfi-13)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

; (check (get-environment-variable "USER") => "da")
(when (os-linux?)
  (check (string-prefix? "/" (get-environment-variable "HOME"))
         => #t
  ) ;check
) ;when

; Test get-environment-variables
(when (os-linux?)
  (let ((envs (get-environment-variables)))
    ; Check that it returns a list
    (check (list? envs) => #t)
    ; Check that it contains HOME with a value starting with "/"
    (let ((home-env (assoc "HOME" envs)))
      (check (pair? home-env) => #t)
      (check (string-prefix? "/" (cdr home-env)) => #t)
    ) ;let
    ; Check that it contains PATH
    (check (pair? (assoc "PATH" envs)) => #t)
  ) ;let
) ;when

(check-report)
