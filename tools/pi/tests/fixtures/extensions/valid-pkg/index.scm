;; A valid package extension inside a subdirectory returning a factory function
(lambda (api)
  (register-flag! api
    (make-extension-flag-def
      :flag "--pkg-flag"
      :description "Flag registered from valid-pkg/index.scm"
      :handler (lambda (val) val)
    ) ;make-extension-flag-def
  ) ;register-flag!
  "pkg-loaded"
)
