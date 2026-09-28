(import (liii base) (liii argparse))

(let ((parser (make-argument-parser)))
  (parser :add '((name . "width") (type . number) (default . 40)))
  (parser :parse)
  (display (parser 'width))
  (newline)
) ;let
