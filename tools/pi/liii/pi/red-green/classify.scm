;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi red-green classify)
  (import (scheme base) (scheme char) (liii string) (liii list))
  (export pi-tdd-glob-match? pi-tdd-language-of pi-classify-tdd-file)
  (begin

    (define %other-extensions
      '(".json" ".yaml" ".yml" ".toml" ".md" ".txt" ".css" ".scss" ".html"
        ".svg" ".png" ".jpg" ".jpeg" ".gif" ".lock" ".env" ".gitignore"
        ".editorconfig" ".ini" ".cfg" ".xml")
    ) ;define

    (define %config-filenames
      '("package.json" "package-lock.json" "makefile" "dockerfile" "cargo.toml"
        "go.mod" "go.sum" "build.gradle" "build.gradle.kts" "pom.xml"
        "pyproject.toml" "setup.py" "setup.cfg" "requirements.txt"
        "tsconfig.json" "gemfile" "composer.json")
    ) ;define

    (define %config-prefixes
      '("tsconfig." "jsconfig." "vitest.config." "jest.config." "eslint"
        "prettier" "vite.config." "webpack.config." "rollup.config.")
    ) ;define

    (define %test-patterns
      '((scheme "**/*-test.scm" "**/*-test.sld" "**/tests/**/*.scm"
          "**/test_*.scm" "**/*_test.scm")
        (typescript "**/*.test.ts" "**/*.spec.ts" "**/*.test.tsx"
          "**/*.spec.tsx" "**/*.test.js" "**/*.spec.js" "**/*.test.jsx"
          "**/*.spec.jsx")
        (python "**/test_*.py" "**/*_test.py" "**/tests/**/*.py"
          "**/conftest.py")
        (go "**/*_test.go")
        (rust "**/tests/**/*.rs" "**/*_test.rs")
        (java "**/*Test.java" "**/*Spec.java")
        (php "**/*Test.php"))
    ) ;define

    (define (%normalize-path path)
      (let* ((raw (if (string? path) path "")) (slashed (string-replace raw "\\" "/")))
        (cond ((string-starts? slashed "./") (substring slashed 2 (string-length slashed)))
              ((string-starts? slashed "/") (substring slashed 1 (string-length slashed)))
              (else slashed)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%split-segments path)
      (filter (lambda (s) (not (string-null? s))) (string-split path "/"))
    ) ;define

    (define (%segment-match? pattern segment)
      (let ((plen (string-length pattern)) (slen (string-length segment)))
        (let loop
          ((p-idx 0) (s-idx 0))
          (cond ((>= p-idx plen) (= s-idx slen))
                ((char=? (string-ref pattern p-idx) #\*)
                 (or (loop (+ p-idx 1) s-idx) (and (< s-idx slen) (loop p-idx (+ s-idx 1))))
                ) ;
                ((>= s-idx slen) #f)
                ((or (char=? (string-ref pattern p-idx) #\?)
                   (char=? (string-ref pattern p-idx) (string-ref segment s-idx))
                 ) ;or
                 (loop (+ p-idx 1) (+ s-idx 1))
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%match-segments pattern-segs path-segs)
      (cond ((null? pattern-segs) (null? path-segs))
            ((string=? (car pattern-segs) "**")
             (or (%match-segments (cdr pattern-segs) path-segs)
               (and (pair? path-segs) (%match-segments pattern-segs (cdr path-segs)))
             ) ;or
            ) ;
            ((null? path-segs) #f)
            ((%segment-match? (car pattern-segs) (car path-segs))
             (%match-segments (cdr pattern-segs) (cdr path-segs))
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    ;; Glob match with native-pi semantics: a pattern may match any path suffix.
    (define (pi-tdd-glob-match? path pattern)
      (let* ((path-segs (%split-segments (%normalize-path path)))
             (pattern-segs (%split-segments pattern))
            ) ;
        (let loop
          ((suffix path-segs))
          (cond ((%match-segments pattern-segs suffix) #t)
                ((null? suffix) #f)
                (else (loop (cdr suffix)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (pi-tdd-language-of path)
      (let ((lower (string-downcase (%normalize-path path))))
        (cond ((or (string-ends? lower ".scm")
                 (string-ends? lower ".sld")
                 (string-ends? lower ".ss")
               ) ;or
               'scheme
              ) ;
              ((or (string-ends? lower ".ts")
                 (string-ends? lower ".tsx")
                 (string-ends? lower ".js")
                 (string-ends? lower ".jsx")
                 (string-ends? lower ".mjs")
                 (string-ends? lower ".cjs")
               ) ;or
               'typescript
              ) ;
              ((string-ends? lower ".py") 'python)
              ((string-ends? lower ".go") 'go)
              ((string-ends? lower ".rs") 'rust)
              ((string-ends? lower ".java") 'java)
              ((string-ends? lower ".php") 'php)
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%other-extension? path)
      (let ((lower (string-downcase path)))
        (any (lambda (ext) (string-ends? lower ext)) %other-extensions)
      ) ;let
    ) ;define

    (define (%config-file? path)
      (let* ((segs (%split-segments path))
             (name
               (if (pair? segs) (string-downcase (car (reverse segs))) "")
             ) ;name
            ) ;
        (or (member name %config-filenames)
          (any (lambda (prefix) (string-starts? name prefix)) %config-prefixes)
        ) ;or
      ) ;let*
    ) ;define

    (define (pi-classify-tdd-file path)
      (let* ((normalized (%normalize-path path)) (language (pi-tdd-language-of normalized)))
        (cond ((%other-extension? normalized) 'other)
              ((%config-file? normalized) 'other)
              ((not language) 'other)
              (else
                (let ((patterns (cdr (assoc language %test-patterns))))
                  (if (any (lambda (pattern) (pi-tdd-glob-match? normalized pattern)) patterns)
                    'test
                    'implementation
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
