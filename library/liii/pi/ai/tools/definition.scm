;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai tools definition)
  (import (scheme base) (liii base))
  (export pi-tool-definition pi-tool-definition? pi-tool-definition-name
    pi-tool-definition-description pi-tool-definition-parameters
    pi-tool-definition-execute pi-tool-definition-snippet
    pi-tool-definition-guidelines
  ) ;export
  (begin

    (define*-record-type <pi-tool-definition>
      (pi-tool-definition name
        description
        parameters
        execute
        (snippet "")
        (guidelines '())
      ) ;pi-tool-definition
      pi-tool-definition?
      (name pi-tool-definition-name)
      (description pi-tool-definition-description)
      (parameters pi-tool-definition-parameters)
      (execute pi-tool-definition-execute)
      (snippet pi-tool-definition-snippet)
      (guidelines pi-tool-definition-guidelines)
    ) ;define*-record-type

  ) ;begin
) ;define-library
