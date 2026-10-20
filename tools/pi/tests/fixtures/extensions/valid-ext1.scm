;; A valid extension returning a factory function
(lambda (api)
  (register-tool! api
    (make-extension-tool-def
      :name "test-tool-1"
      :description "Tool 1 from valid-ext1"
      :execute (lambda (args) "result-1")
    ) ;make-extension-tool-def
  ) ;register-tool!
  "ext1-loaded"
)
