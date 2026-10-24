;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code extension install)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii path)
    (liii os)
    (liii subprocess)
    (liii error)
    (liii either)
    (liii pi code read)
    (liii pi code failure)
  ) ;import
  (export pi-parse-git-extension-name pi-valid-extension-name?
    pi-resolve-install-target pi-git-clone-extension pi-install-extension
  ) ;export
  (begin

    (define (pi-parse-git-extension-name url)
      (if (not (string? url))
        #f
        (let* ((trimmed (string-trim-both url)) (len (string-length trimmed)))
          (if (zero? len)
            #f
            (let* (
                    ;; 去除末尾斜杠
                    (s
                      (if
                        (and (> len 1)
                          (or (char=? (string-ref trimmed (- len 1)) #\/)
                            (char=? (string-ref trimmed (- len 1)) #\\)
                          ) ;or
                        ) ;and
                        (substring trimmed 0 (- len 1))
                        trimmed
                      ) ;if
                    ) ;s
                    ;; 提取最后一个路径段（兼容 / 与 \ 与 :）
                    (last-seg
                      (let loop
                        ((i (- (string-length s) 1)))
                        (cond ((< i 0) s)
                              ((or (char=? (string-ref s i) #\/)
                                 (char=? (string-ref s i) #\\)
                                 (char=? (string-ref s i) #\:)
                               ) ;or
                               (substring s (+ i 1) (string-length s))
                              ) ;
                              (else (loop (- i 1)))
                        ) ;cond
                      ) ;let
                    ) ;last-seg
                  ) ;
              (if (string-null? last-seg)
                #f
                ;; 如果以 .git 结尾则剥除
                (if (and (> (string-length last-seg) 4)
                      (string-ends? (string-downcase last-seg) ".git")
                    ) ;and
                  (substring last-seg 0 (- (string-length last-seg) 4))
                  last-seg
                ) ;if
              ) ;if
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-valid-extension-name? name)
      (and (string? name)
        (not (string-null? name))
        (string-every (lambda (ch)
                        (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\-) (char=? ch #\_))
                      ) ;lambda
          name
        ) ;string-every
      ) ;and
    ) ;define

    (define (pi-resolve-install-target name . opts)
      (unless (string? name)
        (type-error "pi-resolve-install-target: name must be a string")
      ) ;unless
      (unless (pi-valid-extension-name? name)
        (value-error (string-append "Invalid extension name: " name))
      ) ;unless
      (let* ((local? (pi-keyword-value :local? opts #f))
             (agent-dir (pi-keyword-value :agent-dir opts (pi-get-agent-dir)))
             (cwd-dir (pi-keyword-value :cwd opts (getcwd)))
             (base-dir (if local?
                         (path-join (path cwd-dir) ".pi" "extensions")
                         (path-join (path agent-dir) "extensions")
                       ) ;if
             ) ;base-dir
             (target-path (path-join base-dir name))
            ) ;
        (path->string (path-resolve target-path))
      ) ;let*
    ) ;define

    (define (%rmdir-rec d)
      (let ((p (if (path? d) d (path d))))
        (when (path-dir? p)
          (for-each
            (lambda (e)
              (let ((full (path-join p e)))
                (if (path-dir? full) (%rmdir-rec full) (remove (path->string full)))
              ) ;let
            ) ;lambda
            (vector->list (path-list p))
          ) ;for-each
          (rmdir (path->string p))
        ) ;when
      ) ;let
    ) ;define

    (define (pi-git-clone-extension url dest-dir-str . opts)
      (let* ((timeout (pi-keyword-value :timeout opts 120))
             (dest-path (path dest-dir-str))
             (parent-dir (path-parent dest-path))
            ) ;
        (when (path-exists? dest-path)
          (value-error (string-append "Target extension directory already exists: " dest-dir-str)
          ) ;value-error
        ) ;when
        ;; 确保父级 extensions 目录存在
        (unless (path-exists? parent-dir)
          (path-mkdir parent-dir :parents #t :exist-ok #t)
        ) ;unless
        (let ((res (run-either (list "git" "clone" url dest-dir-str)
                     :timeout timeout
                     :stdout  'capture
                     :stderr  'capture
                   ) ;run-either
              ) ;res
             ) ;
          (if (either-right? res)
            #t
            (let ((err-info (to-left res)))
              ;; 失败时清理半成品
              (when (path-exists? dest-path)
                (catch #t
                  (lambda ()
                    (if (path-dir? dest-path) (%rmdir-rec dest-path) (remove dest-dir-str))
                  ) ;lambda
                  (lambda args #f)
                ) ;catch
              ) ;when
              (error 'install-error
                (string-append "Failed to clone extension from "
                  url
                  ": "
                  (pi-command-failure-detail err-info)
                ) ;string-append
              ) ;error
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (pi-install-extension url . opts)
      (let* ((ext-name (pi-parse-git-extension-name url)))
        (unless (and ext-name (pi-valid-extension-name? ext-name))
          (value-error (string-append "Cannot infer valid extension name from URL: "
                         (object->string url)
                       ) ;string-append
          ) ;value-error
        ) ;unless
        (let* ((target-dir (apply pi-resolve-install-target ext-name opts))
               (timeout (pi-keyword-value :timeout opts 120))
              ) ;
          (pi-git-clone-extension url target-dir :timeout timeout)
          target-dir
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
