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

(import (liii check)
        (liii argparse)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "name") (type . string) (short . "n") (default . "anonymous"))
  ) ;parser
  (check (parser 'name) => "anonymous")
  (parser :parse-args '("--name" "john"))
  (check (parser 'name) => "john")
) ;let

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "width") (type . number) (short . "width") (default . 80))
  ) ;parser

  (check (parser :get-argument "width") => 80)

  (parser :parse-args '("--width" "100"))
  (check (parser :get-argument "width") => 100)
  (check (parser 'width) => 100)

  (parser :parse-args '("-width" "60"))
  (check (parser 'width) => 60)
) ;let

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "height") (type . number) (default . 60))  ; without short name
  ) ;parser
  (parser :parse-args '("--height" "120"))
  (check (parser :get-argument "height") => 120)
) ;let

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "width") (type . number) (short . "w") (default . 80))
  ) ;parser
  (parser :add-argument
    '((name . "title") (type . string) (default . "Untitled"))
  ) ;parser
  (parser :parse-args '("-w" "100" "--title" "My Document"))
  (check (parser :get-argument "width") => 100)
  (check (parser :get-argument "title") => "My Document")
) ;let

(let1 parser (make-argument-parser)
  (check-catch 'type-error (parser :add-argument '((name name))))
  (check-catch 'value-error (parser :add-argument '()))
) ;let1

(check-report)

