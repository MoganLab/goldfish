;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii path)
  (export path path? path-copy path-copy-into path-dir? path-file? path-exists?
    path-getsize path-read-text path-read-lines path-read-bytes path-write-text
    path-write-bytes path-append-text path-touch path-root path-of-drive
    path-from-parts path-from-env path-cwd path-home path-temp-dir path-parts
    path-type path-drive path->string path-from-string path-name path-stem
    path-suffix path-suffixes path-with-name path-with-stem path-with-suffix
    path-relative-to path-starts-with? path-equals? path=? path-absolute?
    path-relative? path-join path-parent path-parents path-list path-list-path
    path-rmdir path-unlink path-rename path-mkdir path-absolute path-expanduser
    path-match path-as-posix path-resolve path-glob path-rglob
  ) ;export
  (import (liii path record) (liii path pure) (liii path fs) (liii path glob))
) ;define-library
