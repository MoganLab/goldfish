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

(define-library (liii tui loader)
  (export spinner-presets spinner-frames-resolve spinner-status-icon
    make-spinner spinner? make-loader loader? spinner-frames spinner-frame-index
    spinner-current-frame spinner-text spinner-set-text! spinner-prefix
    spinner-set-prefix! spinner-suffix spinner-set-suffix! spinner-interval
    spinner-set-interval! spinner-visible? spinner-set-visible! spinner-show!
    spinner-hide! spinner-status spinner-set-status! spinner-success!
    spinner-fail! spinner-error! spinner-warn! spinner-stop! spinner-tick!
    spinner-reset!
  ) ;export
  (import (liii tui loader presets) (liii tui loader spinner))
) ;define-library
