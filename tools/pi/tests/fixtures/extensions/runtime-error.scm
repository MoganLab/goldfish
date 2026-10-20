;; Deliberate runtime error when factory is executed
(lambda (api)
  (error 'extension-runtime-error "Deliberate error during extension execution")
)
