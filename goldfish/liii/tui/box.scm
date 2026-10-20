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

(define-library (liii tui box)
  (export make-box box? make-vstack vstack? make-hstack hstack? <border-style>
    make-border-style border-style? border-style-name border-style-tl
    border-style-tr border-style-bl border-style-br border-style-h
    border-style-v border-style-resolve border-render-top border-render-bottom
    border-render-line
  ) ;export
  (import (liii tui box border)
    (liii tui box box)
    (liii tui box vstack)
    (liii tui box hstack)
  ) ;import
) ;define-library
