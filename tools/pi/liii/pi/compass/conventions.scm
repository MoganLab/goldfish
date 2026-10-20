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

(define-library (liii pi compass conventions)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi compass types)
    (liii pi compass packages)
  ) ;import
  (export PI_COMPASS_MAX_CONVENTION_CHARS PI_COMPASS_CONVENTION_FILES
    pi-compass-detect-conventions pi-compass-truncate-convention
  ) ;export
  (begin

    (define PI_COMPASS_MAX_CONVENTION_CHARS 2000)

    (define PI_COMPASS_CONVENTION_FILES
      '("AGENTS.md" "CLAUDE.md" "GEMINI.md" ".editorconfig" "eslint.config.js"
        "eslint.config.ts" "eslint.config.mjs" ".eslintrc.json" ".eslintrc.js"
        ".eslintrc.yml" "prettier.config.js" "prettier.config.ts" ".prettierrc"
        ".prettierrc.json" "biome.json" "biome.jsonc" "rustfmt.toml"
        ".golangci.yml" ".golangci.yaml" "CONTRIBUTING.md" ".stylelintrc.json"
        "deno.json" "deno.jsonc")
    ) ;define

    (define (pi-compass-truncate-convention text max-length)
      (if (<= (string-length text) max-length)
        text
        (string-append (substring text 0 max-length) "\n... (truncated)")
      ) ;if
    ) ;define

    (define (pi-compass-detect-conventions cwd)
      (let loop
        ((files PI_COMPASS_CONVENTION_FILES) (acc '()))
        (if (null? files)
          (reverse acc)
          (let* ((file (car files))
                 (content
                   (pi-compass-read-text (path->string (path-join (path cwd) file)))
                 ) ;content
                ) ;
            (if (string? content)
              (loop (cdr files)
                (cons (pi-compass-convention file
                        (pi-compass-truncate-convention content PI_COMPASS_MAX_CONVENTION_CHARS)
                      ) ;pi-compass-convention
                  acc
                ) ;cons
              ) ;loop
              (loop (cdr files) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
