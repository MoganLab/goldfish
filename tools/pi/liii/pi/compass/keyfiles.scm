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

(define-library (liii pi compass keyfiles)
  (import (scheme base) (liii list) (liii path) (liii pi compass types))
  (export PI_COMPASS_KEY_FILES pi-compass-detect-key-files)
  (begin

    (define PI_COMPASS_KEY_FILES
      '(("README.md" "Project documentation")
        ("LICENSE" "License file")
        ("LICENSE.md" "License file")
        ("CHANGELOG.md" "Release history")
        ("CONTRIBUTING.md" "Contribution guidelines")
        ("SECURITY.md" "Security policy")
        ("ARCHITECTURE.md" "Architecture documentation")
        ("AGENTS.md" "AI agent conventions")
        ("CLAUDE.md" "Claude Code instructions")
        (".env.example" "Environment variable template")
        (".env.sample" "Environment variable template")
        ("Dockerfile" "Container build definition")
        ("docker-compose.yml" "Container orchestration")
        ("docker-compose.yaml" "Container orchestration")
        ("Makefile" "Build automation")
        (".github/workflows" "GitHub Actions CI/CD")
        (".gitlab-ci.yml" "GitLab CI/CD")
        ("Jenkinsfile" "Jenkins pipeline"))
    ) ;define

    (define (pi-compass-detect-key-files cwd)
      (let loop
        ((entries PI_COMPASS_KEY_FILES) (acc '()))
        (if (null? entries)
          (reverse acc)
          (let* ((entry (car entries))
                 (rel (car entry))
                 (description (cadr entry))
                 (full (path->string (path-join (path cwd) rel)))
                ) ;
            (loop (cdr entries)
              (if (path-exists? (path full))
                (cons (pi-compass-key-file rel description) acc)
                acc
              ) ;if
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
