(import (liii check)
        (liii pi compass tree)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

;; pi-compass-format-directory-tree
;; 目录名后带 /；末项用 └──，其余用 ├──；子层缩进用 │   或空格。
(define tree
  (list (pi-compass-directory-entry "a"
          "dir"
          (list (pi-compass-directory-entry "b" "file" #f))
        ) ;pi-compass-directory-entry
    (pi-compass-directory-entry "c" "file" #f)
  ) ;list
) ;define

(check (pi-compass-format-directory-tree tree)
  =>
  "├── a/\n│   └── b\n└── c"
) ;check

(define nested
  (list (pi-compass-directory-entry "x"
          "dir"
          (list (pi-compass-directory-entry "y"
                  "dir"
                  (list (pi-compass-directory-entry "z" "file" #f))
                ) ;pi-compass-directory-entry
          ) ;list
        ) ;pi-compass-directory-entry
  ) ;list
) ;define

(check (pi-compass-format-directory-tree nested)
  =>
  "└── x/\n    └── y/\n        └── z"
) ;check

(check (pi-compass-format-directory-tree '()) => "")

(check-report)
