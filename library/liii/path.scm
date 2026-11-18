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
  ;; 构造函数
  (export path path-of-drive path-from-parts path-from-env path-cwd
    path-home path-temp-dir
  ) ;export
  ;; 谓词
  (export path? path-dir? path-file? path-exists? path-absolute?
    path-relative? path-equals? path=? path-starts-with?
  ) ;export
  ;; 属性访问
  (export path-parts path-type path-drive path-root path-name path-stem
    path-suffix path-suffixes path-getsize
  ) ;export
  ;; 转换
  (export path->string path-as-posix
  ) ;export
  ;; 纯路径计算
  (export path-with-name path-with-stem path-with-suffix path-relative-to
    path-join path-parent path-parents path-absolute path-expanduser
    path-resolve path-realpath
  ) ;export
  ;; 读写操作
  (export path-read-text path-read-lines path-read-bytes path-write-text
    path-write-bytes path-append-text path-touch
  ) ;export
  ;; 目录操作与通配检索
  (export path-list path-list-path path-rmdir path-mkdir path-match
    path-glob path-rglob
  ) ;export
  ;; 文件管理
  (export path-copy path-copy-into path-unlink path-rename
  ) ;export
  (import (liii path record) (liii path pure) (liii path fs) (liii path glob))
) ;define-library
