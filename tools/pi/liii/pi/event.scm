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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi event)
  (import (scheme base) (liii pi types))
  (export pi-iso-8601-utc-now pi-current-time-millis pi-serialize-event
    pi-event-session pi-event-agent-start pi-event-turn-start
    pi-event-message-start pi-event-message-update pi-event-message-end
    pi-event-tool-execution-start pi-event-tool-execution-update
    pi-event-tool-execution-end pi-event-turn-end pi-event-agent-end
    pi-event-agent-settled pi-build-json-event
  ) ;export
) ;define-library
