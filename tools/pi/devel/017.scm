(import (scheme base)
  (scheme file)
  (scheme read)
  (liii check)
  (liii subprocess)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; [017] 测试去除计划文档依赖 验收脚本

;; 验收点 1: fixtures 自建素材存在且内容非空
(check-true (file-exists? "tools/pi/tests/fixtures/sample.txt"))
(let ((sample-content
        (call-with-input-file "tools/pi/tests/fixtures/sample.txt"
          (lambda (p)
            (let loop
              ((acc '()))
              (let ((l (read-line p)))
                (if (eof-object? l) (string-join (reverse acc) "\n") (loop (cons l acc)))
              ) ;let
            ) ;let
          ) ;lambda
        ) ;call-with-input-file
      ) ;sample-content
     ) ;
  (check-true (> (string-length (string-trim sample-content)) 0))
  (check-true (string-contains sample-content "hello sample"))
) ;let

;; 验收点 2: 全仓库测试文件无 PI_PLAN/PI_SPEC/CLAUDE_CODE_PLAN 引用（git grep 断言 0 命中）
;; 2.1 检查 tools/pi/tests/ 目录下的所有文件
(check (run "sh -c '! git grep -E \"PI_PLAN|PI_SPEC|CLAUDE_CODE_PLAN\" tools/pi/tests/'"
         :timeout 30
       ) ;run
  =>
  0
) ;check

;; 2.2 检查全仓库所有测试文件（*test*.scm）
(check (run "sh -c '! git grep -E \"PI_PLAN|PI_SPEC|CLAUDE_CODE_PLAN\" -- \"*test*.scm\"'"
         :timeout 30
       ) ;run
  =>
  0
) ;check

;; 2.3 检查曾经引用过的 4 个测试文件
(check (run "sh -c '! git grep -E \"PI_PLAN\" tools/pi/tests/liii/pi/pi-execute-find-test.scm tools/pi/tests/liii/pi/pi-execute-grep-test.scm tools/pi/tests/liii/pi/pi-execute-read-test.scm tools/pi/tests/liii/pi/pi-resolve-path-test.scm'"
         :timeout 30
       ) ;run
  =>
  0
) ;check

;; 验收点 3: 临时改名 PI_PLAN.md 后，相关 4 个测试仍然全部通过
(check (run "sh -c 'mv PI_PLAN.md PI_PLAN.md.tmp && trap \"mv PI_PLAN.md.tmp PI_PLAN.md\" EXIT; bin/gf test tools/pi/tests/liii/pi/pi-execute-find-test.scm && bin/gf test tools/pi/tests/liii/pi/pi-execute-grep-test.scm && bin/gf test tools/pi/tests/liii/pi/pi-execute-read-test.scm && bin/gf test tools/pi/tests/liii/pi/pi-resolve-path-test.scm'"
         :timeout 120
       ) ;run
  =>
  0
) ;check

;; 验证改名后已正常恢复
(check-true (file-exists? "PI_PLAN.md"))
(check-false (file-exists? "PI_PLAN.md.tmp"))

;; 验收点 4: 临时改名 PI_PLAN.md 后，tools/pi/tests 全量测试仍全部通过
(check (run "sh -c 'mv PI_PLAN.md PI_PLAN.md.tmp && trap \"mv PI_PLAN.md.tmp PI_PLAN.md\" EXIT; for f in tools/pi/tests/liii/*-test.scm tools/pi/tests/liii/pi/*-test.scm; do bin/gf test \"$f\" >/dev/null 2>&1 || exit 1; done'"
         :timeout 300
       ) ;run
  =>
  0
) ;check

;; 验证改名后已正常恢复
(check-true (file-exists? "PI_PLAN.md"))
(check-false (file-exists? "PI_PLAN.md.tmp"))

;; 验收点 5: 门禁检查通过
(check (run "bin/gf check" :timeout 120) => 0)

(check-report)
