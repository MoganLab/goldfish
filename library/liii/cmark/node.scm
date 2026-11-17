
(define-library (liii cmark node)
  (import (scheme base) (liii base))
  (export make-cmark-node)
  (export cmark-node?)
  (export cmark-node-type)
  (export cmark-node-set-type!)
  (export cmark-node-data)
  (export cmark-node-set-data!)
  (export cmark-node-children)
  (export cmark-node-set-children!)
  (export cmark-node-parent)
  (export cmark-node-set-parent!)
  (export *cmark-block-types*)
  (export *cmark-inline-types*)
  (export cmark-block-type?)
  (export cmark-inline-type?)
  (export cmark-all-types)
  (export make-cmark-document)
  (export make-cmark-block-quote)
  (export make-cmark-alert)
  (export make-cmark-paragraph)
  (export make-cmark-heading)
  (export make-cmark-code-block)
  (export make-cmark-thematic-break)
  (export make-cmark-table)
  (export make-cmark-text)
  (export make-cmark-softbreak)
  (export make-cmark-linebreak)
  (export make-cmark-code)
  (export make-cmark-emoji)
  (export make-cmark-emph)
  (export make-cmark-strong)
  (export make-cmark-strikeout)
  (export make-cmark-link)
  (export make-cmark-image)
  (export make-cmark-list)
  (export make-cmark-item)
  (export make-cmark-html-block)
  (export make-cmark-html-inline)
  (export make-cmark-math-block)
  (export make-cmark-math-inline)
  (export cmark-node-append-child!)
  (export cmark-node-first-child)
  (export cmark-node-last-child)
  (export cmark-node-type?)
  (export cmark-document?)
  (export cmark-paragraph?)
  (export cmark-text?)
  (export cmark-heading?)
  (export cmark-list?)
  (begin
    (define*-record-type <cmark-node>
      (make-cmark-node type (data '()) (children '()) (parent #f))
      cmark-node?
      (type cmark-node-type cmark-node-set-type!)
      (data cmark-node-data cmark-node-set-data!)
      (children cmark-node-children cmark-node-set-children!)
      (parent cmark-node-parent cmark-node-set-parent!)
    ) ;define*-record-type

    (define *cmark-block-types*
      '(document block_quote alert list item code_block html_block custom_block
         paragraph heading thematic_break table math_block)
    ) ;define

    (define *cmark-inline-types*
      '(text softbreak linebreak code emoji html_inline custom_inline emph
         strong strikeout link image math_inline)
    ) ;define

    (define (cmark-block-type? type)
      (if (memq type *cmark-block-types*) #t #f)
    ) ;define

    (define (cmark-inline-type? type)
      (if (memq type *cmark-inline-types*) #t #f)
    ) ;define

    (define (cmark-all-types)
      (append *cmark-block-types* *cmark-inline-types*)
    ) ;define

    (define (make-cmark-document)
      (make-cmark-node 'document)
    ) ;define

    (define (make-cmark-block-quote)
      (make-cmark-node 'block_quote)
    ) ;define

    (define (make-cmark-paragraph)
      (make-cmark-node 'paragraph)
    ) ;define

    (define (make-cmark-alert kind)
      (make-cmark-node 'alert `((kind . ,kind)))
    ) ;define

    (define (make-cmark-heading level)
      (make-cmark-node 'heading `((level . ,level)))
    ) ;define

    (define (make-cmark-code-block info literal)
      (make-cmark-node 'code_block `((info . ,info) (literal . ,literal)))
    ) ;define

    (define (make-cmark-thematic-break)
      (make-cmark-node 'thematic_break)
    ) ;define

    (define (make-cmark-table alignments header rows)
      (make-cmark-node 'table
        `((alignments . ,alignments) (header . ,header) (rows . ,rows))
      ) ;make-cmark-node
    ) ;define

    (define (make-cmark-text content)
      (make-cmark-node 'text `((content . ,content)))
    ) ;define

    (define (make-cmark-softbreak)
      (make-cmark-node 'softbreak)
    ) ;define

    (define (make-cmark-linebreak)
      (make-cmark-node 'linebreak)
    ) ;define

    (define (make-cmark-code literal)
      (make-cmark-node 'code `((literal . ,literal)))
    ) ;define

    (define (make-cmark-emoji shortcode)
      (make-cmark-node 'emoji `((shortcode . ,shortcode)))
    ) ;define

    (define (make-cmark-emph)
      (make-cmark-node 'emph)
    ) ;define

    (define (make-cmark-strong)
      (make-cmark-node 'strong)
    ) ;define

    (define (make-cmark-strikeout)
      (make-cmark-node 'strikeout)
    ) ;define

    (define (make-cmark-link url title)
      (make-cmark-node 'link `((url . ,url) (title . ,title)))
    ) ;define

    (define (make-cmark-image url title)
      (make-cmark-node 'image `((url . ,url) (title . ,title)))
    ) ;define

    (define (make-cmark-list list-type start delimiter)
      (make-cmark-node 'list
        `((type . ,list-type) (start . ,start) (delimiter . ,delimiter))
      ) ;make-cmark-node
    ) ;define

    (define (make-cmark-item)
      (make-cmark-node 'item)
    ) ;define

    (define (make-cmark-html-block literal)
      (make-cmark-node 'html_block `((literal . ,literal)))
    ) ;define

    (define (make-cmark-html-inline literal)
      (make-cmark-node 'html_inline `((literal . ,literal)))
    ) ;define

    (define (make-cmark-math-block literal)
      (make-cmark-node 'math_block `((literal . ,literal)))
    ) ;define

    (define (make-cmark-math-inline literal)
      (make-cmark-node 'math_inline `((literal . ,literal)))
    ) ;define

    (define (cmark-node-append-child! parent child)
      (cmark-node-set-children! parent
        (append (cmark-node-children parent) (list child))
      ) ;cmark-node-set-children!
      (cmark-node-set-parent! child parent)
    ) ;define

    (define (cmark-node-first-child node)
      (if (null? (cmark-node-children node)) #f (car (cmark-node-children node)))
    ) ;define

    (define (cmark-node-last-child node)
      (if (null? (cmark-node-children node))
        #f
        (car (reverse (cmark-node-children node)))
      ) ;if
    ) ;define

    (define (cmark-node-type? node type)
      (eq? (cmark-node-type node) type)
    ) ;define

    (define (cmark-document? node)
      (cmark-node-type? node 'document)
    ) ;define

    (define (cmark-paragraph? node)
      (cmark-node-type? node 'paragraph)
    ) ;define

    (define (cmark-text? node)
      (cmark-node-type? node 'text)
    ) ;define

    (define (cmark-heading? node)
      (cmark-node-type? node 'heading)
    ) ;define

    (define (cmark-list? node)
      (cmark-node-type? node 'list)
    ) ;define
  ) ;begin
) ;define-library
