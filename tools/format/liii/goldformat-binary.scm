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

;; 跨平台定位 clang-format 二进制路径。
;; Windows 直接用 PATH 中的 clang-format；macOS 用 homebrew 的 llvm@19；
;; Linux 按版本号优先探测已安装的 clang-format-19，回退到通用名。

(define-library (liii goldformat-binary)
  (import (liii base) (liii os))
  (export clang-format-binary)
  (begin

    (define (clang-format-binary)
      (cond ((os-windows?) "clang-format")
            ((os-macos?) "/opt/homebrew/opt/llvm@19/bin/clang-format")
            (else (let loop
                    ((paths '("/usr/local/bin/clang-format-19"
                              "/usr/lib/llvm-19/bin/clang-format"
                              "/usr/bin/clang-format-19"
                              "/usr/bin/clang-format")
                     ) ;paths
                    ) ;
                    (if (null? paths)
                      "clang-format"
                      (if (file-exists? (car paths)) (car paths) (loop (cdr paths)))
                    ) ;if
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
