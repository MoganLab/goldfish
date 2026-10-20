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

(define-library (liii tui terminal)
  (export <terminal> make-terminal-record terminal? terminal-size terminal-write
    terminal-cursor terminal-set-cursor! terminal-clear terminal-clear-line
    terminal-cursor-visible? terminal-hide-cursor! terminal-show-cursor!
    terminal-flush terminal-detect-size make-virtual-terminal virtual-terminal?
    virtual-terminal-screen virtual-terminal-raw virtual-terminal-resize!
    make-standard-terminal standard-terminal?
  ) ;export
  (import (liii tui terminal protocol)
    (liii tui terminal size)
    (liii tui terminal virtual)
    (liii tui terminal standard)
  ) ;import
) ;define-library
