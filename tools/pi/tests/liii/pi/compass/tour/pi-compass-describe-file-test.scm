(import (liii check)
        (liii string)
        (liii pi compass tour)
) ;import

(check-set-mode! 'report-failed)

;; pi-compass-describe-file：纯文件名启发式，不读文件内容。
(check (pi-compass-describe-file "tests/login.test.js") => "Test file for login")
(check (pi-compass-describe-file "tests/login-test.scm") => "Test file for login")
(check (pi-compass-describe-file "tests/base.spec.js") => "Test file for base")
(check (pi-compass-describe-file "src/index.js") => "Entry point for src module")
(check (pi-compass-describe-file "index.js") => "Package entry point")
(check (pi-compass-describe-file "src/config.js") => "Configuration: config")
(check (pi-compass-describe-file "src/api/routes.js") => "Routing: routes")
(check (pi-compass-describe-file "src/middleware.js") => "Middleware: middleware")
(check (pi-compass-describe-file "src/user.model.js") => "Data model: user.model")
(check (pi-compass-describe-file "src/user.schema.js") => "Data model: user.schema")
(check (pi-compass-describe-file "src/user.service.js") => "Service: user.service")
(check (pi-compass-describe-file "src/home.controller.js") => "Handler: home.controller")
(check (pi-compass-describe-file "src/home.handler.js") => "Handler: home.handler")
(check (pi-compass-describe-file "src/fmt.util.js") => "Utility: fmt.util")
(check (pi-compass-describe-file "src/fmt.helper.js") => "Utility: fmt.helper")
(check (pi-compass-describe-file "src/about.js") => "about module")

(check-report)
