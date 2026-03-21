(import (liii rich-range))

((rich-range 1 10) :for-each
 (lambda (x) (display x) (newline)))

((rich-range :inclusive 1 10) :for-each
 (lambda (x) (display x) (newline)))
