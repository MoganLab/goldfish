;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-entry header)
  (import (scheme base) (liii base))
  (export pi-session-header-entry pi-session-header-entry?
    pi-session-header-entry-version pi-session-header-entry-id
    pi-session-header-entry-timestamp pi-session-header-entry-cwd
  ) ;export
  (begin

    (define*-record-type <pi-session-header-entry>
      (pi-session-header-entry id timestamp cwd (version 3))
      pi-session-header-entry?
      (version pi-session-header-entry-version)
      (id pi-session-header-entry-id)
      (timestamp pi-session-header-entry-timestamp)
      (cwd pi-session-header-entry-cwd)
    ) ;define*-record-type

  ) ;begin
) ;define-library
