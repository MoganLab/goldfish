;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass frameworks)
  (import (scheme base) (liii list) (liii pi compass types))
  (export PI_COMPASS_FRAMEWORK_RULES pi-compass-detect-frameworks)
  (begin

    (define PI_COMPASS_FRAMEWORK_RULES
      '(("next" "Next.js" "definite")
        ("react" "React" "definite")
        ("vue" "Vue" "definite")
        ("@angular/core" "Angular" "definite")
        ("svelte" "Svelte" "definite")
        ("express" "Express" "definite")
        ("fastify" "Fastify" "definite")
        ("hono" "Hono" "definite")
        ("koa" "Koa" "definite")
        ("nestjs" "NestJS" "likely")
        ("@nestjs/core" "NestJS" "definite")
        ("nuxt" "Nuxt" "definite")
        ("remix" "Remix" "likely")
        ("@remix-run/react" "Remix" "definite")
        ("astro" "Astro" "definite")
        ("gatsby" "Gatsby" "definite")
        ("tailwindcss" "Tailwind CSS" "definite")
        ("prisma" "Prisma" "definite")
        ("@prisma/client" "Prisma" "definite")
        ("drizzle-orm" "Drizzle" "definite")
        ("django" "Django" "definite")
        ("flask" "Flask" "definite")
        ("fastapi" "FastAPI" "definite")
        ("spring-boot" "Spring Boot" "likely")
        ("actix-web" "Actix Web" "definite")
        ("rocket" "Rocket" "definite")
        ("axum" "Axum" "definite")
        ("tokio" "Tokio" "definite")
        ("gin-gonic/gin" "Gin" "definite")
        ("labstack/echo" "Echo" "definite")
        ("gofiber/fiber" "Fiber" "definite")
        ("laravel/framework" "Laravel" "definite")
        ("symfony/framework-bundle" "Symfony" "definite")
        ("vitest" "Vitest" "definite")
        ("jest" "Jest" "definite")
        ("playwright" "Playwright" "definite")
        ("@playwright/test" "Playwright" "definite")
        ("cypress" "Cypress" "definite")
        ("pytest" "pytest" "definite"))
    ) ;define

    (define (%all-dependencies packages)
      (if (null? packages)
        '()
        (append (pi-compass-package-dependencies (car packages))
          (%all-dependencies (cdr packages))
        ) ;append
      ) ;if
    ) ;define

    (define (%detect deps rules seen acc)
      (if (null? rules)
        (reverse acc)
        (let* ((rule (car rules))
               (dep (car rule))
               (name (cadr rule))
               (confidence (caddr rule))
              ) ;
          (if (and (member dep deps) (not (member name seen)))
            (%detect deps
              (cdr rules)
              (cons name seen)
              (cons (pi-compass-framework name confidence dep) acc)
            ) ;%detect
            (%detect deps (cdr rules) seen acc)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-compass-detect-frameworks packages)
      (%detect (%all-dependencies packages) PI_COMPASS_FRAMEWORK_RULES '() '())
    ) ;define

  ) ;begin
) ;define-library
