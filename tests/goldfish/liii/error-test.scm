;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii error)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

(check-catch 'os-error (os-error))

(check-catch 'file-not-found-error (file-not-found-error))

(check-catch 'not-a-directory-error (not-a-directory-error))

(check-catch 'file-exists-error (file-exists-error))

(check-catch 'timeout-error (timeout-error))

(check-catch 'type-error (type-error))
(check-catch 'type-error (type-error "msg"))
(check-catch 'type-error (type-error "msg" "msg2"))

(check-true (type-error? 'type-error))
(check-true (type-error? 'wrong-type-arg))

(check-catch 'key-error (key-error))

(check-catch 'value-error (value-error))

(check-catch 'index-error (index-error))

(check-catch '??? (???))

(check-report)

