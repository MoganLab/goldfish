(import (scheme base) (scheme file) (liii check) (liii os))

(check-set-mode! 'report-failed)

;; [001] 核心库迁移·文件 验收脚本
;;
;; 说明：tests/liii/pi/ 旧路径测试属任务 002 范围（核心库迁移·测试与索引），不在本任务（001）断言。

;; 验收点 1: 旧路径 goldfish/liii/pi.scm 与 goldfish/liii/pi/ 已不存在
(check-false (file-exists? "goldfish/liii/pi.scm"))
(check-false (file-exists? "goldfish/liii/pi"))

;; 验收点 2: 新路径 tools/pi/liii/ 文件齐全
(check-true (file-exists? "tools/pi/liii/pi.scm"))
(check-true (file-exists? "tools/pi/liii/pi/anthropic.scm"))
(check-true (file-exists? "tools/pi/liii/pi/bash.scm"))
(check-true (file-exists? "tools/pi/liii/pi/config.scm"))
(check-true (file-exists? "tools/pi/liii/pi/edit.scm"))
(check-true (file-exists? "tools/pi/liii/pi/event.scm"))
(check-true (file-exists? "tools/pi/liii/pi/find.scm"))
(check-true (file-exists? "tools/pi/liii/pi/grep.scm"))
(check-true (file-exists? "tools/pi/liii/pi/ls.scm"))
(check-true (file-exists? "tools/pi/liii/pi/prompt.scm"))
(check-true (file-exists? "tools/pi/liii/pi/provider.scm"))
(check-true (file-exists? "tools/pi/liii/pi/read.scm"))
(check-true (file-exists? "tools/pi/liii/pi/template.scm"))
(check-true (file-exists? "tools/pi/liii/pi/truncate.scm"))
(check-true (file-exists? "tools/pi/liii/pi/write.scm"))

;; 验收点 3: bin/gf test tools/pi/tests/liii/goldpi-args-test.scm 全过
(check (os-call "sh -c \"bin/gf test tools/pi/tests/liii/goldpi-args-test.scm > /dev/null 2>&1\""
       ) ;os-call
  =>
  0
) ;check

;; 验收点 4: bin/gf pi --help 可用
(check (os-call "sh -c \"bin/gf pi --help > /dev/null 2>&1\"") => 0)

(check-report)
