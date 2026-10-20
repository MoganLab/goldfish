(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass frameworks)
        (liii pi compass packages)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

;; pi-compass-detect-frameworks：依赖规则表匹配 + 同名去重。
(let* ((frameworks (pi-compass-detect-frameworks
                     (pi-compass-detect-packages fixture)
                   ) ;pi-compass-detect-frameworks
       ) ;frameworks
       (names (map pi-compass-framework-name frameworks))
      ) ;
  (check (pair? (member "React" names)) => #t)
  (check (pair? (member "Express" names)) => #t)
  (check (pair? (member "Vitest" names)) => #t)
  (check (pair? (member "Tokio" names)) => #t)
  (check (pair? (member "Axum" names)) => #t)
  (check (pair? (member "FastAPI" names)) => #t)
  (check (pair? (member "pytest" names)) => #t)
  (check (pair? (member "Laravel" names)) => #t)
) ;let*

;; 置信度与来源字段。
(let* ((frameworks (pi-compass-detect-frameworks
                     (pi-compass-detect-packages fixture)
                   ) ;pi-compass-detect-frameworks
       ) ;frameworks
       (react (find (lambda (f) (string=? (pi-compass-framework-name f) "React"))
                frameworks
              ) ;find
       ) ;react
      ) ;
  (check (pi-compass-framework-confidence react) => "definite")
  (check (pi-compass-framework-source react) => "react")
) ;let*

;; 无依赖时为空。
(check (pi-compass-detect-frameworks '()) => '())
(check (pi-compass-detect-frameworks
         (list (pi-compass-package "npm" "x" #f '("left-pad")))
       ) ;pi-compass-detect-frameworks
  =>
  '()
) ;check

;; 同名去重：@prisma/client 与 prisma 都命中 Prisma，只保留一条。
(let* ((pkgs (list (pi-compass-package "npm" "x" #f '("prisma" "@prisma/client")))
       ) ;pkgs
       (ns (map pi-compass-framework-name
             (pi-compass-detect-frameworks pkgs)
           ) ;map
       ) ;ns
      ) ;
  (check ns => '("Prisma"))
) ;let*

(check-report)
