(import (liii base) (liii argparse))

(let1 parser (make-argument-parser)
  (parser :add
	  '((name . "width") (type . number) (default . 40)))
  (parser :parse)
  (display* (parser 'width) "\n"))
