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

;; (scheme lazy) library for R7RS
;; stdmod.tex 导出清单：delay delay-force force make-promise promise?
;;
;; delay / delay-force 由 (scheme base) 以扩展形式导出；
;; force / make-promise / promise? 由实现库 (goldfish) 提供
;; （见 expander/kernel/substrate.scm 与 liii/host-abi.scm）。

(define-library (scheme lazy)
  (import (scheme base) (goldfish))
  (export delay
    delay-force
    force
    make-promise
    promise?
  ) ;export
  (begin
  ) ;begin
) ;define-library
