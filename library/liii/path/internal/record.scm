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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii path internal record)
  (export %make-path-record %path? %path-record-parts %path-record-type
    %path-record-drive %path-record-root
  ) ;export
  (import (scheme base))
  (begin

    ;; ;============================================================
    ;; ; Path record 类型
    ;; ;============================================================
    ;; ; root 字段：#\\ / #\/ 表示有根分隔符(绝对路径或驱动器根),
    ;; ; #f 表示无根(drive-relative 或相对路径)。用于区分 C:\foo
    ;; ; (root=#\\)和 C:foo(root=#f)的语义差异。
    (define-record-type <path>
      (%make-path-record parts type drive root)
      %path?
      (parts %path-record-parts)
      (type %path-record-type)
      (drive %path-record-drive)
      (root %path-record-root)
    ) ;define-record-type

  ) ;begin
) ;define-library
