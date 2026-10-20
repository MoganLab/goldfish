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
    parse-keys <terminal> make-terminal-record terminal? terminal-size
    terminal-write terminal-cursor terminal-set-cursor! terminal-clear
    terminal-clear-line terminal-cursor-visible? terminal-hide-cursor!
    terminal-show-cursor! terminal-flush terminal-detect-size
    make-virtual-terminal virtual-terminal? virtual-terminal-screen
    virtual-terminal-raw virtual-terminal-resize! make-standard-terminal
    standard-terminal? make-box box? make-vstack vstack? make-hstack hstack?
    <border-style> make-border-style border-style? border-style-resolve
    make-spinner spinner? make-loader loader? spinner-tick! spinner-reset!
    spinner-current-frame spinner-frame-index spinner-text spinner-set-text!
    spinner-prefix spinner-set-prefix! spinner-suffix spinner-set-suffix!
    spinner-interval spinner-set-interval! spinner-visible? spinner-set-visible!
    spinner-show! spinner-hide! spinner-status spinner-set-status!
    spinner-success! spinner-fail! spinner-error! spinner-warn! spinner-stop!
    spinner-presets spinner-frames-resolve
  ) ;export
  (import (liii tui component)
    (liii tui text-width)
    (liii tui keys)
    (liii tui terminal)
    (liii tui box)
    (liii tui loader)
  ) ;import
) ;define-library
