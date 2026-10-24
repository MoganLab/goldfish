(import (liii check)
        (liii string)
        (liii pi code simplify scope)
) ;import

(check-set-mode! 'report-failed)

(define (file path status ranges available?)
  (list (cons 'path path)
    (cons 'status status)
    (cons 'ranges ranges)
    (cons 'ranges-available? available?)
  ) ;list
) ;define

;; pi-build-scope-lines
;; 按原生 pi-simplify 的措辞生成 Scope 行。

;; 1. 多区间：相邻数字折叠成 start-end，单行只写数字
(check (pi-build-scope-lines (list (file "src/calc.ts" "modified"
                                           '(((start . 21) (end . 33))
                                             ((start . 40) (end . 40)))
                                           #t
                                         ) ;list
                                   ) ;list
       ) ;pi-build-scope-lines
  =>
  "- src/calc.ts (modified; changed lines: 21-33, 40)"
) ;check

;; 2. 新增文件整文件在范围
(check (pi-build-scope-lines (list (file "src/util.ts" "added" #f #t)))
  =>
  "- src/util.ts (added; entire file is in scope)"
) ;check

;; 3. 纯删除：没有可简化的当前行
(check (pi-build-scope-lines (list (file "src/old.ts" "modified" '() #t)))
  =>
  "- src/old.ts (modified; deletions only — no current lines to simplify)"
) ;check

;; 4. diff 失败：行范围不可用
(check (pi-build-scope-lines (list (file "src/broken.ts" "modified" #f #f)))
  =>
  "- src/broken.ts (modified; changed lines unavailable — inspect git diff before editing)"
) ;check

;; 5. 重命名 / 复制保留状态名
(check (pi-build-scope-lines (list (file "src/new.ts" "renamed"
                                           '(((start . 5) (end . 5)))
                                           #t
                                         ) ;list
                                   ) ;list
       ) ;pi-build-scope-lines
  =>
  "- src/new.ts (renamed; changed lines: 5)"
) ;check

;; 6. 多文件以换行连接；空列表为空串
(check (pi-build-scope-lines (list (file "a.ts" "added" #f #t)
                                   (file "b.ts" "modified" '(((start . 2) (end . 3)))
                                         #t
                                       ) ;list
                             ) ;list
       ) ;pi-build-scope-lines
  =>
  "- a.ts (added; entire file is in scope)\n- b.ts (modified; changed lines: 2-3)"
) ;check
(check (pi-build-scope-lines '()) => "")

(check-report)
