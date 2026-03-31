;
; Copyright (C) 2026 The Goldfish Scheme Authors
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

(define-library (liii golddoc)
  (import (scheme base)
          (liii golddoc-args)
          (liii golddoc-library)
          (liii golddoc-function)
          (liii golddoc-cli)
  ) ;import
  (export parse-doc-args
          library-query?
          parse-library-query
          excluded-test-group?
          find-visible-library-root
          find-tests-root-for-load-root
          library-doc-path
          exported-name->test-stem
          function-doc-path
          run-golddoc
          main
  ) ;export
  (begin

    (define (main)
      (run-golddoc)
    ) ;define

  ) ;begin
) ;define-library
