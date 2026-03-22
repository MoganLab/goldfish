(import (liii base) (liii argparse))

(let ((parser (make-argument-parser)))
  (parser :add
	  '((name . "width") (type . number) (default . 40))
  ) ;parser
  (parser :parse)
  (display* (parser 'width) "\n")
) ;let
