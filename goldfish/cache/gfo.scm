(define (gfo-dir)
  (let ((xdg (getenv "XDG_CACHE_HOME")))
    (string-append
      (if (and xdg (not (string=? xdg ""))) xdg
        (string-append (or (getenv "HOME") "/tmp") "/.cache"))
      "/goldfish/ccache")))

(define (gfo-separator? c)
  (or (char=? c #\/) (char=? c #\\)))

(define (gfo-key path)
  (let ((n (string-length path)))
    (let loop ((i 0) (start 0) (parts '()))
      (if (> i n)
        (if (null? parts)
          "root"
          (let ((rev (reverse parts)))
            (let lp ((acc (car rev)) (rest (cdr rev)))
              (if (null? rest)
                acc
                (lp (string-append acc "/" (car rest)) (cdr rest))))))
        (if (or (= i n) (gfo-separator? (string-ref path i)))
          (let ((comp (substring path start i)))
            (loop (+ i 1) (+ i 1)
                  (if (or (string=? comp "")
                          (string=? comp ".")
                          (and (> (string-length comp) 0)
                               (char=? (string-ref comp (- (string-length comp) 1)) #\:)))
                    parts
                    (cons (if (string=? comp "..") "_dotdot" comp) parts))))
          (loop (+ i 1) start parts))))))

(define (gfo-path path)
  (string-append (gfo-dir) "/" (gfo-key path) ".gfo"))

(define (gfo-ensure-parent! dir file)
  (if (not (file-exists? dir)) (g_mkdir dir))
  (let ((rel (substring file (string-length dir))))
    (let ((n (string-length rel)))
      (let loop ((i 1))
        (let ((j (let lp ((k i))
                   (if (or (= k n) (char=? (string-ref rel k) #\/))
                     k
                     (lp (+ k 1))))))
          (when (< j n)
            (let ((d (string-append dir (substring rel 0 j))))
              (if (not (file-exists? d)) (g_mkdir d))
              (loop (+ j 1)))))))))

(define (gfo-stamp path)
  (list (g_path-getmtime path) (g_path-getsize path)))

(define (gfo-valid? gfo-file stamp)
  (and (file-exists? gfo-file)
       (let ((rec (call-with-input-file gfo-file (lambda (p) (car (read-forms p))))))
         (and (pair? rec) (eq? (car rec) 'gfo) (equal? (cadr rec) stamp)))))

(define (gfo-read gfo-file)
  (let ((rec (car (read-forms (open-input-file gfo-file)))))
    (caddr rec)))

(define (gfo-write! gfo-file stamp payload)
  (if (getenv "GOLDFISH_CACHE_READONLY") #f
      (begin
        (gfo-ensure-parent! (gfo-dir) gfo-file)
        (let ((old-length (*s7* 'print-length)))
          (let-set! *s7* 'print-length 1000000)
          (let ((tmp (string-append gfo-file ".tmp")))
            (call-with-output-file tmp
              (lambda (p)
                (if (defined? 'write-roundtrip) (write-roundtrip (list 'gfo stamp payload) p)
                    (write (list 'gfo stamp payload) p))))
            (g_rename tmp gfo-file))
          (let-set! *s7* 'print-length old-length)))))
