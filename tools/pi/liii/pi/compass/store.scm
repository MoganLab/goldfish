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

(define-library (liii pi compass store)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii pi compass types)
    (liii pi compass generate)
  ) ;import
  (export pi-compass-default-codemap-path pi-compass-resolve-path
    pi-compass-load-codemap pi-compass-save-codemap! pi-compass-cache-entry
    pi-compass-get-or-generate pi-compass-cache-hit?
  ) ;export
  (begin

    (define (pi-compass-default-codemap-path cwd)
      (path->string (path-join (path cwd) ".pi" "codemap.json"))
    ) ;define

    (define (pi-compass-resolve-path raw cwd)
      (if (path-absolute? (path raw))
        (path->string (path raw))
        (path->string (path-join (path cwd) raw))
      ) ;if
    ) ;define

    (define (%ensure-parent! file-path)
      (let ((parent (path-parent (path file-path))))
        (unless (path-exists? parent)
          (path-mkdir parent :parents #t :exist-ok #t)
        ) ;unless
      ) ;let
    ) ;define

    (define (pi-compass-load-codemap file-path)
      (if (not (path-file? (path file-path)))
        #f
        (let ((parsed
                (catch #t
                  (lambda () (string->json (path-read-text (path file-path))))
                  (lambda _ #f)
                ) ;catch
              ) ;parsed
             ) ;
          (if (and (pi-compass-object? parsed)
                (pi-compass-object? (pi-compass-jref parsed "data" #f))
              ) ;and
            parsed
            #f
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (pi-compass-save-codemap! file-path entry)
      (%ensure-parent! file-path)
      (path-write-text (path file-path) (json->string entry))
      file-path
    ) ;define

    (define (pi-compass-cache-entry codemap)
      (list (cons "data" codemap)
        (cons "contentHash" (pi-compass-codemap-content-hash codemap))
        (cons "createdAt" (pi-compass-codemap-generated-at codemap))
      ) ;list
    ) ;define

    (define (pi-compass-cache-hit? entry cwd)
      (and entry
        (equal? (pi-compass-jref entry "contentHash" "") (pi-compass-content-hash cwd))
      ) ;and
    ) ;define

    ;; 返回 ("codemap" . codemap) + ("fromCache" . bool) 的关联表。
    (define (pi-compass-get-or-generate cwd project file-path . maybe-refresh)
      (let* ((refresh (if (pair? maybe-refresh) (car maybe-refresh) #f))
             (entry (if refresh #f (pi-compass-load-codemap file-path)))
            ) ;
        (if (pi-compass-cache-hit? entry cwd)
          (list (cons "codemap" (pi-compass-jref entry "data" #f)) (cons "fromCache" #t))
          (let ((codemap (pi-compass-generate-codemap cwd project)))
            (pi-compass-save-codemap! file-path (pi-compass-cache-entry codemap))
            (list (cons "codemap" codemap) (cons "fromCache" #f))
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
