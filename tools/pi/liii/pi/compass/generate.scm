;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass generate)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii hashlib)
    (liii pi time)
    (liii pi compass types)
    (liii pi compass project)
    (liii pi compass tree)
    (liii pi compass packages)
    (liii pi compass frameworks)
    (liii pi compass entry)
    (liii pi compass build)
    (liii pi compass conventions)
    (liii pi compass keyfiles)
  ) ;import
  (export PI_COMPASS_HASH_FILES
    pi-compass-content-hash
    pi-compass-generate-codemap
  ) ;export
  (begin

    (define PI_COMPASS_HASH_FILES
      '("package.json" "package-lock.json" "tsconfig.json" "go.mod" "go.sum"
        "Cargo.toml" "Cargo.lock" "pyproject.toml" "composer.json"
        "composer.lock" "pom.xml" "build.gradle" "build.gradle.kts")
    ) ;define

    (define (%manifest-signature cwd)
      (let loop
        ((files PI_COMPASS_HASH_FILES) (acc '()))
        (if (null? files)
          (string-join (reverse acc) "\n")
          (let* ((file (car files))
                 (full (path->string (path-join (path cwd) file)))
                 (content (pi-compass-read-text full))
                ) ;
            (loop (cdr files)
              (if (string? content) (cons (string-append file "\n" content) acc) acc)
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 哈希覆盖 codemap 真正依赖的输入：深度 2 目录树 + manifest 文件内容。
    (define (pi-compass-content-hash cwd)
      (let* ((tree (pi-compass-build-directory-tree cwd 2))
             (tree-sig (pi-compass-format-directory-tree tree))
             (manifest-sig (%manifest-signature cwd))
             (input (string-append tree-sig "\n--manifests--\n" manifest-sig))
            ) ;
        (substring (sha256 input) 0 16)
      ) ;let*
    ) ;define

    (define (pi-compass-generate-codemap cwd project)
      (let* ((packages (pi-compass-detect-packages cwd))
             (tree (pi-compass-build-directory-tree cwd 2))
            ) ;
        (pi-compass-codemap (pi-compass-project-id project)
          (pi-compass-project-name project)
          (pi-iso-8601-utc-now)
          (pi-compass-content-hash cwd)
          tree
          packages
          (pi-compass-detect-frameworks packages)
          (pi-compass-detect-entry-points cwd packages)
          (pi-compass-detect-build-scripts cwd)
          (pi-compass-detect-conventions cwd)
          (pi-compass-detect-key-files cwd)
        ) ;pi-compass-codemap
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
