;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass types)
  (import (scheme base) (liii string) (liii list))
  (export pi-compass-null? pi-compass-object? pi-compass-jref pi-compass->list
    pi-compass-vector pi-compass-directory-entry pi-compass-entry-name
    pi-compass-entry-type pi-compass-entry-children pi-compass-entry-dir?
    pi-compass-package pi-compass-package-manager pi-compass-package-name
    pi-compass-package-version pi-compass-package-dependencies
    pi-compass-framework pi-compass-framework-name
    pi-compass-framework-confidence pi-compass-framework-source
    pi-compass-entry-point pi-compass-entry-point-path
    pi-compass-entry-point-kind pi-compass-build-script
    pi-compass-build-script-name pi-compass-build-script-command
    pi-compass-build-script-source pi-compass-convention
    pi-compass-convention-source pi-compass-convention-content
    pi-compass-key-file pi-compass-key-file-path pi-compass-key-file-description
    pi-compass-codemap pi-compass-codemap? pi-compass-codemap-project-id
    pi-compass-codemap-project-name pi-compass-codemap-generated-at
    pi-compass-codemap-content-hash pi-compass-codemap-tree
    pi-compass-codemap-packages pi-compass-codemap-frameworks
    pi-compass-codemap-entry-points pi-compass-codemap-build-scripts
    pi-compass-codemap-conventions pi-compass-codemap-key-files
    pi-compass-tour-step pi-compass-tour pi-compass-tour-project-id
    pi-compass-tour-topic pi-compass-tour-generated-at pi-compass-tour-steps
    pi-compass-step-file pi-compass-step-description
  ) ;export
  (begin

    (define (pi-compass-null? x)
      (eq? x 'null)
    ) ;define

    ;; JSON 对象统一表示为字符串键关联表；空对象解析为 '(()。
    (define (pi-compass-object? x)
      (and (list? x) (pair? x) (pair? (car x)))
    ) ;define

    (define (pi-compass-jref obj key default)
      (if (pi-compass-object? obj)
        (let ((pair (assoc key obj)))
          (if pair (cdr pair) default)
        ) ;let
        default
      ) ;if
    ) ;define

    (define (pi-compass->list x)
      (cond ((vector? x) (vector->list x))
            ((list? x) x)
            (else '())
      ) ;cond
    ) ;define

    (define (pi-compass-vector x)
      (if (vector? x) x (list->vector (if (list? x) x '())))
    ) ;define

    (define (pi-compass-directory-entry name type children)
      (append (list (cons "name" name) (cons "type" type))
        (if children (list (cons "children" (pi-compass-vector children))) '())
      ) ;append
    ) ;define

    (define (pi-compass-entry-name e)
      (pi-compass-jref e "name" "")
    ) ;define

    (define (pi-compass-entry-type e)
      (pi-compass-jref e "type" "file")
    ) ;define

    (define (pi-compass-entry-children e)
      (pi-compass->list (pi-compass-jref e "children" #()))
    ) ;define

    (define (pi-compass-entry-dir? e)
      (string=? (pi-compass-entry-type e) "dir")
    ) ;define

    (define (pi-compass-package manager name version dependencies)
      (append (list (cons "manager" manager) (cons "name" name))
        (if (and (string? version) (not (string-null? version)))
          (list (cons "version" version))
          '()
        ) ;if
        (list (cons "dependencies" (pi-compass-vector dependencies)))
      ) ;append
    ) ;define

    (define (pi-compass-package-manager p)
      (pi-compass-jref p "manager" "")
    ) ;define

    (define (pi-compass-package-name p)
      (pi-compass-jref p "name" "")
    ) ;define

    (define (pi-compass-package-version p)
      (let ((v (pi-compass-jref p "version" #f)))
        (if (string? v) v #f)
      ) ;let
    ) ;define

    (define (pi-compass-package-dependencies p)
      (pi-compass->list (pi-compass-jref p "dependencies" #()))
    ) ;define

    (define (pi-compass-framework name confidence source)
      (list (cons "name" name) (cons "confidence" confidence) (cons "source" source))
    ) ;define

    (define (pi-compass-framework-name f)
      (pi-compass-jref f "name" "")
    ) ;define

    (define (pi-compass-framework-confidence f)
      (pi-compass-jref f "confidence" "definite")
    ) ;define

    (define (pi-compass-framework-source f)
      (pi-compass-jref f "source" "")
    ) ;define

    (define (pi-compass-entry-point path kind)
      (list (cons "path" path) (cons "kind" kind))
    ) ;define

    (define (pi-compass-entry-point-path ep)
      (pi-compass-jref ep "path" "")
    ) ;define

    (define (pi-compass-entry-point-kind ep)
      (pi-compass-jref ep "kind" "main")
    ) ;define

    (define (pi-compass-build-script name command source)
      (list (cons "name" name) (cons "command" command) (cons "source" source))
    ) ;define

    (define (pi-compass-build-script-name s)
      (pi-compass-jref s "name" "")
    ) ;define

    (define (pi-compass-build-script-command s)
      (pi-compass-jref s "command" "")
    ) ;define

    (define (pi-compass-build-script-source s)
      (pi-compass-jref s "source" "")
    ) ;define

    (define (pi-compass-convention source content)
      (list (cons "source" source) (cons "content" content))
    ) ;define

    (define (pi-compass-convention-source c)
      (pi-compass-jref c "source" "")
    ) ;define

    (define (pi-compass-convention-content c)
      (pi-compass-jref c "content" "")
    ) ;define

    (define (pi-compass-key-file path description)
      (list (cons "path" path) (cons "description" description))
    ) ;define

    (define (pi-compass-key-file-path kf)
      (pi-compass-jref kf "path" "")
    ) ;define

    (define (pi-compass-key-file-description kf)
      (pi-compass-jref kf "description" "")
    ) ;define

    (define (pi-compass-codemap project-id project-name generated-at
              content-hash tree packages frameworks entry-points build-scripts
              conventions key-files
            ) ;pi-compass-codemap
      (list (cons "projectId" project-id)
        (cons "projectName" project-name)
        (cons "generatedAt" generated-at)
        (cons "contentHash" content-hash)
        (cons "directoryTree" (pi-compass-vector tree))
        (cons "packages" (pi-compass-vector packages))
        (cons "frameworks" (pi-compass-vector frameworks))
        (cons "entryPoints" (pi-compass-vector entry-points))
        (cons "buildScripts" (pi-compass-vector build-scripts))
        (cons "conventions" (pi-compass-vector conventions))
        (cons "keyFiles" (pi-compass-vector key-files))
      ) ;list
    ) ;define

    (define (pi-compass-codemap? cm)
      (and (pi-compass-object? cm) (string? (pi-compass-jref cm "projectId" #f)))
    ) ;define

    (define (pi-compass-codemap-project-id cm)
      (pi-compass-jref cm "projectId" "")
    ) ;define

    (define (pi-compass-codemap-project-name cm)
      (pi-compass-jref cm "projectName" "")
    ) ;define

    (define (pi-compass-codemap-generated-at cm)
      (pi-compass-jref cm "generatedAt" "")
    ) ;define

    (define (pi-compass-codemap-content-hash cm)
      (pi-compass-jref cm "contentHash" "")
    ) ;define

    (define (pi-compass-codemap-tree cm)
      (pi-compass->list (pi-compass-jref cm "directoryTree" #()))
    ) ;define

    (define (pi-compass-codemap-packages cm)
      (pi-compass->list (pi-compass-jref cm "packages" #()))
    ) ;define

    (define (pi-compass-codemap-frameworks cm)
      (pi-compass->list (pi-compass-jref cm "frameworks" #()))
    ) ;define

    (define (pi-compass-codemap-entry-points cm)
      (pi-compass->list (pi-compass-jref cm "entryPoints" #()))
    ) ;define

    (define (pi-compass-codemap-build-scripts cm)
      (pi-compass->list (pi-compass-jref cm "buildScripts" #()))
    ) ;define

    (define (pi-compass-codemap-conventions cm)
      (pi-compass->list (pi-compass-jref cm "conventions" #()))
    ) ;define

    (define (pi-compass-codemap-key-files cm)
      (pi-compass->list (pi-compass-jref cm "keyFiles" #()))
    ) ;define

    (define (pi-compass-tour-step file description)
      (list (cons "file" file) (cons "description" description))
    ) ;define

    (define (pi-compass-step-file s)
      (pi-compass-jref s "file" "")
    ) ;define

    (define (pi-compass-step-description s)
      (pi-compass-jref s "description" "")
    ) ;define

    (define (pi-compass-tour project-id topic generated-at steps)
      (list (cons "projectId" project-id)
        (cons "topic" topic)
        (cons "generatedAt" generated-at)
        (cons "steps" (pi-compass-vector steps))
      ) ;list
    ) ;define

    (define (pi-compass-tour-project-id t)
      (pi-compass-jref t "projectId" "")
    ) ;define

    (define (pi-compass-tour-topic t)
      (pi-compass-jref t "topic" "")
    ) ;define

    (define (pi-compass-tour-generated-at t)
      (pi-compass-jref t "generatedAt" "")
    ) ;define

    (define (pi-compass-tour-steps t)
      (pi-compass->list (pi-compass-jref t "steps" #()))
    ) ;define

  ) ;begin
) ;define-library
