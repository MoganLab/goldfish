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

(define-library (liii pi tui)
  (export make-pi-header pi-header? make-pi-footer pi-footer? make-pi-app
    pi-app? pi-app-screen pi-app-editor pi-app-render! pi-app-run
  ) ;export
  (import (liii pi tui header) (liii pi tui footer) (liii pi tui app))
) ;define-library
