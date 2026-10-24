(import (liii check)
        (liii string)
        (liii pi code simplify ranges)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-changed-ranges
;; 解析 git diff --unified=0 的新文件侧 hunk 头，并合并相邻区间。

;; 1. 基本 hunk 头：省略行数视为 1；零行 hunk（纯删除）跳过
(check (pi-parse-changed-ranges
         "@@ -99,2 +100,3 @@\n@@ -104 +105,2 @@\n@@ -200 +202,0 @@\n"
       ) ;pi-parse-changed-ranges
  =>
  '(((start . 100) (end . 102)) ((start . 105) (end . 106)))
) ;check

;; 2. 相邻区间合并
(check (pi-parse-changed-ranges "@@ -1 +10,2 @@\n@@ -5 +12,3 @@\n")
  =>
  '(((start . 10) (end . 14)))
) ;check

;; 3. 重叠区间合并
(check (pi-parse-changed-ranges "@@ -1 +10,5 @@\n@@ -2 +12,2 @@\n")
  =>
  '(((start . 10) (end . 14)))
) ;check

;; 4. 不相邻区间保持分离；函数签名信息不影响解析
(check (pi-parse-changed-ranges
         "@@ -1,2 +3,2 @@ function foo()\n@@ -20 +30,3 @@ function bar()\n"
       ) ;pi-parse-changed-ranges
  =>
  '(((start . 3) (end . 4)) ((start . 30) (end . 32)))
) ;check

;; 5. 非 hunk 行与空输出
(check (pi-parse-changed-ranges "diff --git a/x b/x\nindex abc..def\n") => '())
(check (pi-parse-changed-ranges "") => '())

;; 6. 带空格的 hunk 头（git 偶尔输出 @@  -1 +2 @@ 形式）
(check (pi-parse-changed-ranges "@@  -1 +7,2  @@\n")
  =>
  '(((start . 7) (end . 8)))
) ;check

(check-report)
