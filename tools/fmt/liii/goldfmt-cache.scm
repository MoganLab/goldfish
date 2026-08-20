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

;; 格式化缓存：供 scheme-fmt / cpp-fmt 共享。
;; 缓存键为文件内容的 SHA-256；缓存目录按 Goldfish 版本隔离。

(define-library (liii goldfmt-cache)
  (import (liii base) (liii path) (liii hashlib))
  (export fmt-cache-base-dir fmt-cache-path fmt-cache-hit? fmt-cache-touch)
  (begin
    (load-source-file "cache/gfo.scm")
    (define (fmt-cache-base-dir)
      (string-append (gfo-dir) "/fmt/" (version)))
    ;; 统一 gfo 缓存：~/.cache/goldfish/ccache/fmt/<version>/<path>.gfo
    (define (fmt-cache-path file-path)
      (gfo-path (string-append "fmt/" (version) "/" file-path))
    ) ;define

    (define (fmt-cache-hit? file-path)
      (gfo-valid? (fmt-cache-path file-path) (gfo-stamp file-path))
    ) ;define

    (define (fmt-cache-touch file-path)
      (gfo-write! (fmt-cache-path file-path) (gfo-stamp file-path) 'fmt)
    ) ;define

  ) ;begin
) ;define-library
