(import (scheme base) (scheme file) (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; [002] 核心库迁移·测试与索引 验收脚本
;;
;; 验收依据：tools/pi/devel/002.md 中的验收点
;; 1. tests/liii/pi/ 旧路径已不存在
;; 2. tools/pi/tests/liii/pi/ 文件齐全
;; 3. bin/gf test tools/pi/tests/liii/pi/ 下的测试抽样全过
;; 4. bin/gf doc --build-json 可跑
;; 5. bin/gf doc 抽查可用

;; 验收点 1: tests/liii/pi/ 旧路径已不存在，顶层 pi-test.scm 亦不存在于 tests/liii/
(check-false (file-exists? "tests/liii/pi"))
(check-false (file-exists? "tests/liii/pi-test.scm"))

;; 验收点 2: tools/pi/tests/liii/pi/ 文件齐全
(check-true (file-exists? "tools/pi/tests/liii/pi-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-bash-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-read-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-write-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-edit-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-grep-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-find-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-execute-ls-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-load-settings-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-load-auth-test.scm"))
(check-true (file-exists? "tools/pi/tests/liii/pi/gemini-build-request-body-test.scm")
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/anthropic-build-request-body-test.scm")
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/pi-build-json-event-test.scm")
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/uuid4-test.scm"))

;; 验收点 3: bin/gf test tools/pi/tests/liii/pi/ 下的测试抽样全过（必带 :timeout）
(check (run "bin/gf test tools/pi/tests/liii/pi-test.scm" :timeout 60) => 0)
(check (run "bin/gf test tools/pi/tests/liii/pi/pi-execute-bash-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/gemini-build-request-body-test.scm"
         :timeout 60
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/anthropic-build-request-body-test.scm"
         :timeout 60
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/pi-load-settings-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/uuid4-test.scm" :timeout 60)
  =>
  0
) ;check

;; 验收点 4: bin/gf doc --build-json 可跑（必带 :timeout）
(check (run "bin/gf doc --build-json" :timeout 60) => 0)

;; 验收点 5: bin/gf doc 抽查可用（必带 :timeout）
(check (run "bin/gf doc substring" :timeout 60) => 0)
(check (run "bin/gf doc scheme/base" :timeout 60) => 0)

(check-report)
