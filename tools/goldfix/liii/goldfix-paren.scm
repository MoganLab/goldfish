(define-library (liii goldfix-paren)
  (import (scheme base))
  (import (liii goldfix-env))
  (import (liii goldfix-line))
  (import (liii goldfix-list))
  (import (liii goldfix-paren-core))
  (import (liii goldfix-paren-right-tag-plan))
  (import (liii goldfix-paren-right-tag-insert))

  (export make-env env?)
  (export env-tag env-lparen-line env-lparen-col env-parent)
  (export env-children env-set-children!)
  (export env-rparen-line env-set-rparen-line!)

  (export env-end-line)
  (export find-next-sibling)
  (export find-insert-position)

  (export insert-line-at)
  (export sort-envs-for-insertion)
  (export insert-single-line-of-right-tag)

  (export make-right-tag-line)

  (export remove-orphan-right-paren-lines)

  (export remove-rparens-from-right)
  (export line-is-multi-line-start?)
  (export fix-env-parens)
  (export get-multi-line-envs)
  (export remove-rparens-from-right-by-diff)
  (export add-rparens-by-diff)
) ;define-library
