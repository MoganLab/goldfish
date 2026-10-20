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

(define-library (liii tui)
  (export <component> make-component component? component-render
    component-handle-input component-invalidate! make-text make-truncated-text
    make-spacer strip-ansi display-width truncate-to-width wrap-text parse-key
    parse-keys
  ) ;export
  (import (liii tui component) (liii tui text-width) (liii tui keys))
) ;define-library
