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

;; 无参数 gf 的默认工具只从用户级配置加载，不受当前项目注册影响。
(define-library (liii gfproject default)
  (import (scheme base) (liii gfproject config) (liii gfproject tools))
  (export gfproject-run-default-tool)
  (begin

    (define (%not-registered)
      (display "Error: The default 'pi' command is not registered in ~/.gf.\n"
        (current-error-port)
      ) ;display
      (display "Install the latest gfcode with:\n" (current-error-port))
      (display "  gf get -g https://github.com/MoganLab/gfcode.git --ref @latest\n"
        (current-error-port)
      ) ;display
      (display "For help with gf itself, run:\n  gf help\n" (current-error-port))
      1
    ) ;define

    (define (gfproject-run-default-tool command . opt-gf-lib)
      (let* ((bundle (apply gfproject-resolve-tool-bundle command opt-gf-lib))
             (user-tool (and bundle (bundle-ref bundle "user-tool")))
             (gf-lib (gfproject--opt-gf-lib opt-gf-lib))
             (base-dir (and bundle (gfproject-tool-base-dir bundle)))
            ) ;
        (if (and bundle (bundle-ref bundle "has-user-tool"))
          (gfproject-prepare-and-run-tool* command user-tool gf-lib #f base-dir)
          (%not-registered)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
