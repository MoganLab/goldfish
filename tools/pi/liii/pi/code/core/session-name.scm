;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话显示名：session_info 元数据持久化与启动名称恢复
(define-library (liii pi code core session-name)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii uuid)
    (liii pi ai types)
    (liii pi code cli args)
    (liii pi code core session-manager)
  ) ;import
  (export append-session-info! get-session-name resolve-initial-session-name
    setup-session-file-with-name
  ) ;export
  (begin

    (define (append-session-info! file-path name)
      (let ((normalized (pi-normalize-session-name name)))
        (if (or (not file-path) (not normalized))
          #f
          (let* ((id (uuid4))
                 (parent-id (find-last-session-message-id file-path))
                 (entry (list (cons "type" "session_info")
                          (cons "id" id)
                          (cons "parentId" (or parent-id 'null))
                          (cons "timestamp" (pi-iso-8601-utc-now))
                          (cons "name" normalized)
                        ) ;list
                 ) ;entry
                ) ;
            (append-session-entry! file-path (json->string entry))
            id
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (get-session-name file-path)
      (if (or (not file-path) (not (file-exists? file-path)))
        #f
        (let ((name #f))
          (for-each
            (lambda (line)
              (let ((entry (catch #t (lambda () (string->json (string-trim line))) (lambda _ #f))))
                (when
                  (and entry
                    (assoc "type" entry)
                    (equal? (cdr (assoc "type" entry)) "session_info")
                  ) ;and
                  (set! name (pi-normalize-session-name (cdr (assoc "name" entry))))
                ) ;when
              ) ;let
            ) ;lambda
            (path-read-lines file-path)
          ) ;for-each
          name
        ) ;let
      ) ;if
    ) ;define

    (define (setup-session-file-with-name parsed cfg cwd)
      (let-values (((sfile sdir) (setup-session-file parsed cfg cwd)))
        (let ((name (pi-args-ref parsed 'name)))
          (when (and sfile name)
            (append-session-info! sfile name)
          ) ;when
        ) ;let
        (values sfile sdir)
      ) ;let-values
    ) ;define

    (define (resolve-initial-session-name parsed cfg cwd)
      (or (pi-args-ref parsed 'name)
        (and (not (pi-args-ref parsed 'no-session))
          (let ((source (resolve-resume-session-file parsed (resolve-session-dir parsed cfg cwd))
                ) ;source
               ) ;
            (and source (get-session-name source))
          ) ;let
        ) ;and
      ) ;or
    ) ;define

  ) ;begin
) ;define-library
