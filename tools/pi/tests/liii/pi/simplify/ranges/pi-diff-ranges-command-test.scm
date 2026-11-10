;; pi-diff-ranges-command
;; 构建获取单个文件行改动的 git diff 命令行字符串。
;;
;; 语法
;; ----
;; (pi-diff-ranges-command ref staged path)
;;
;; 参数
;; ----
;; ref : string?
;; Git 引用或提交哈希。
;;
;; staged : boolean?
;; 是否比较暂存区。
;;
;; path : string?
;; 目标文件路径。
;;
;; 返回值
;; ----
;; string?
;; 完整的 git diff 命令字符串。

(import (liii check) (liii string) (liii pi simplify ranges))

(check-set-mode! 'report-failed)

;; 逐文件获取改动行范围；staged 使用 --cached，回退 ref 时忽略 --staged。

(check (pi-diff-ranges-command "HEAD" #f "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff HEAD -- src/a.ts"
) ;check

(check (pi-diff-ranges-command "HEAD" #t "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff --cached -- src/a.ts"
) ;check

(check (pi-diff-ranges-command "HEAD~1" #t "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff HEAD~1 -- src/a.ts"
) ;check

(check (pi-diff-ranges-command "main" #f "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff main -- src/a.ts"
) ;check

;; 不安全的 ref / path 使用单引号包裹
(check-true (string-contains? (pi-diff-ranges-command "main; rm -rf /tmp/x" #f "a b.ts")
              "'main; rm -rf /tmp/x'"
            ) ;string-contains?
) ;check-true
(check-true (string-contains? (pi-diff-ranges-command "HEAD" #f "a b.ts") "'a b.ts'")
) ;check-true

(check-report)
