(import (scheme base) (scheme file) (liii check) (liii subprocess) (liii path))

(check-set-mode! 'report-failed)

;; [021] CI 多平台通过 验收脚本

;; 验收点 1: tests/goldfish/liii/ 下旧布局残留测试已删除
(check-false (file-exists? "tests/goldfish/liii/pi-test.scm"))
(check-false (file-exists? "tests/goldfish/liii/pi-anthropic-test.scm"))

;; 确保 tests/goldfish/liii/ 下无任何 pi* 文件
(check (run "sh -c '! ls tests/goldfish/liii/pi* 2>/dev/null'" :timeout 10)
  =>
  0
) ;check

;; 验收点 2: bin/gf test --all 全过
(check (run "bin/gf test --all" :timeout 300) => 0)

;; 验收点 3: tools/pi 单测全部通过
(check (run "sh -c 'for f in tools/pi/tests/liii/*-test.scm tools/pi/tests/liii/pi/*-test.scm; do [ -f \"$f\" ] && (bin/gf test \"$f\" >/dev/null 2>&1 || exit 1); done'"
         :timeout 300
       ) ;run
  =>
  0
) ;check

;; 验收点 4: 门禁检查通过
(check (run "bin/gf check" :timeout 120) => 0)

(check-report)
