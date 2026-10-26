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

(define-library (liii goldfix cache)
  (export fix-cache-base-dir fix-cache-path fix-cache-hit? fix-cache-touch)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii hashlib)
    (liii os)
  ) ;import

  (begin

    (define (fix-cache-base-dir)
      (path->string (path-join (path-home) ".cache" "goldfish" "fix" (version)))
    ) ;define

    (define (fix-cache-path file-path)
      (let* ((hash (sha256-by-file file-path))
             (prefix (substring hash 0 2))
             (rest (substring hash 2))
             (base (fix-cache-base-dir))
            ) ;
        (path->string (path-join base prefix rest))
      ) ;let*
    ) ;define

    (define (fix-cache-hit? file-path)
      (let ((cache (fix-cache-path file-path)))
        (file-exists? cache)
      ) ;let
    ) ;define

    (define (fix-cache-touch file-path)
      (let* ((cache (fix-cache-path file-path))
             (cache-dir (path->string (path-parent (path cache))))
            ) ;
        (unless (path-dir? (path cache-dir))
          (mkdir cache-dir)
        ) ;unless
        (path-touch (path cache))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
