;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core skills)
  (export %make-pi-skill pi-skill? pi-skill-name pi-skill-description
    pi-skill-file-path pi-skill-base-dir pi-skill-disable-model-invocation?
    pi-validate-skill-name pi-validate-skill-description parse-frontmatter
    strip-frontmatter pi-load-skills format-skills-for-prompt
    expand-skill-command parse-skill-block
  ) ;export
  (import (liii pi code core skills types)
    (liii pi code core skills frontmatter)
    (liii pi code core skills discovery)
    (liii pi code core skills format)
  ) ;import
) ;define-library
