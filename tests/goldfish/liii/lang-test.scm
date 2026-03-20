;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii lang)
        (only (liii base) let1 identity)
        (liii cut)
        (liii case)
        (liii error)
        (liii list)
) ;import

(define == class=?)
(check-set-mode! 'report-failed)

(define-case-class person
  ((name string? "Bob")
   (age integer?)
  ) ;
) ;define-case-class

(check (class=? (list 1 2) (list 1 2)) => #t)
(check (class=? (box 10) 10) => #t)  
(check (class=? 10 (box 10)) => #t)  
(check (class=? (box 10) (box 10)) => #t)  
(check (class=? 10 10) => #t)  
(check-true (class=? (person "Bob" 21) (person "Bob" 21)))

(let ()
  (define-case-class person ((name string?) (country string?))
    (chained-define (@default)
      (person "Andy" "China")
    ) ;chained-define
    (chained-define (%set-country! c)
      (set! country c)
      (%this)
    ) ;chained-define
    (chained-define (%set-name! n)
      (set! name n)
      (%this)
    ) ;chained-define
    (chained-define (%set-both! n c)
      (%this :set-name! n :set-country! c)
    ) ;chained-define
    (chained-define (%to-string)
      (rich-string (format #f "Hello ~a from ~a" name country))
    ) ;chained-define
  ) ;define-case-class
  (check (person :default :to-string :get) => "Hello Andy from China")
  (check (person :default :set-both! "Bob" "Russia" :to-string :get) => "Hello Bob from Russia")
  (check ((person "Alice" "Japan") :set-name! "Lily" :to-string :get) => "Hello Lily from Japan")
) ;let

(check
  (with-output-to-string
    (lambda ()
      (display* "hello world" "\n")
    ) ;lambda
  ) ;with-output-to-string
  => "hello world\n"
) ;check

(let1 bob (person "Bob" 21)
  (check (object->string bob) => "(person :name \"Bob\" :age 21)")
) ;let1

(check (object->string 42) => "42")
(check (object->string "hello") => "\"hello\"")
(check (object->string #\a) => "#\\a")
(check (object->string '(1 2 3)) => "(1 2 3)")
(check (object->string #(1 2 3)) => "#(1 2 3)")

(check (box 42) => (rich-integer 42))
(check (box 3.14) => (rich-float 3.14))
(check (box #\a) => (rich-char 97))
(check (box "hello") => (rich-string "hello"))
(check (box '(1 2 3)) => (rich-list '(1 2 3)))
(check (box #(1 2 3)) => (rich-vector #(1 2 3)))
(check (box (hash-table 'a 1 'b 2)) => (rich-hash-table (hash-table 'a 1 'b 2)))
(check-catch 'type-error (box #t))

(check ($ 1 :to 3) => '(1 2 3))
(check ($ "hello world" :replace "world" "suger" :index-of "suger") => 6)
(check ($ '(1 2 3) :empty?) => #f)

(check
 (($ 100 :to 128)
  :take 10
  :map (@ + _ 1)
  :filter even?
  :collect
 ) ;
 => '(102 104 106 108 110)
) ;check

(check ($ 42 :get) => 42)

(check-true ($ 42 :equals ($ 42)))
(check-false ($ 41 :equals ($ 42)))

(check (($ 1 :to 2) :collect) => (list 1 2))
(check (($ 1 :to 1) :collect) => (list 1))
(check (($ 2 :to 1) :collect) => (list ))

(check (($ 1 :until 3) :collect) => (list 1 2))
(check (($ 1 :until 2) :collect) => (list 1))
(check (($ 2 :until 2) :collect) => (list ))

(check ($ 65 :to-rich-char) => #\A)
(check-catch 'value-error ($ #x110000 :to-rich-char))

(check ($ 1 :to-string) => "1")
(check ($ -10 :to-string) => "-10")
(check ($ 0 :to-string) => "0")

(check (+ 1 (rich-integer :max-value)) => (rich-integer :min-value))

(check (- (rich-integer :min-value) 1) => (rich-integer :max-value))

(check ($ 0 :sqrt) => 0)       
(check ($ 1 :sqrt) => 1)       
(check ($ 2 :sqrt) => 1)       
(check ($ 9 :sqrt) => 3)       
(check ($ 8 :sqrt) => 2)
(check ($ 10 :sqrt) => 3)
(check ($ 144 :sqrt) => 12)       
(check ($ 289 :sqrt) => 17)       
(check ($ 290 :sqrt) => 17)       
(check ($ 10201 :sqrt) => 101)       
(check ($ 10403 :sqrt) => 101) 
(check ($ (rich-integer :max-value) :sqrt) => 3037000499)
(check-catch 'value-error ($ -1 :sqrt))

(check ($ 1/3 :get) => 1/3)
(check ($ 0 :get) => 0)
(check ($ -1/3 :get) => -1/3)

(check ($ 1/3 :abs) => 1/3)
(check ($ 0.0 :abs) => 0.0)
(check ($ -1/3 :abs) => 1/3)

(check ($ 12.2 :get) => 12.2)

(check ($ 1.1 :abs) => 1.1)
(check ($ 0.0 :abs) => 0.0)
(check ($ -1.1 :abs) => 1.1)
(check ($ -inf.0 :abs) => +inf.0)

(check ($ 1.1 :to-string) => "1.1")
(check ($ 0.0 :to-string) => "0.0")
(check ($ -1.2 :to-string) => "-1.2")
(check ($ 1.0 :to-string) => "1.0")

(check ($ 0.0 :sqrt) => 0.0)       
(check ($ 1.0 :sqrt) => 1.0)       
(check ($ 1.44 :sqrt) => 1.2)       
(check ($ 1.69 :sqrt) => 1.3)       
(check-catch 'value-error ($ -1.5 :sqrt))

(check-true ((left "Value error") :left?))
(check-false ((right 1) :left?))

(check-false ((left "Value error") :right?))
(check-true ((right 1) :right?))

(check ((right 1) :get) => 1)
(check ((left "error") :get) => "error")

(check ((right 1) :or-else (right 2)) => (right 1))
(check ((left "error") :get-or-else (right 2)) => (right 2))

(check ((right 1) :get-or-else 2) => 1)
(check ((left "error") :get-or-else 2) => 2)
(check ((left "error") :get-or-else ($ 2)) => ($ 2))

(let1 r ((right 12) :filter-or-else (lambda (x) (> x 10)) -1)
  (check-true (r :right?))
  (check (r :get) => 12)
) ;let1

(let1 r ((right 7) :filter-or-else (lambda (x) (> x 10)) -1)
  (check-true (r :left?))
  (check (r :get) => -1)
) ;let1

(let1 r ((left 7) :filter-or-else (lambda (x) #f) -1)
  (check-true (r :left?))
  (check (r :get) => 7)
) ;let1

(check-true ((right 1) :contains 1))
(check-false ((left "error") :contains 1))
(check-true ((right (box 1)) :contains 1))

;; 测试 1: right 值转换为 option
(let ((e1 (right 42)))
  (check ((e1 :to-option) :get) => 42)
  (check-true ((e1 :to-option) :defined?))
  (check-false ((e1 :to-option) :empty?))
) ;let

;; 测试 2: left 值转换为 option (应该返回 none)
(let ((e2 (left "error")))
  (check ((e2 :to-option) :empty?) => #t)
  (check-false ((e2 :to-option) :defined?))
  (check-catch 'value-error ((e2 :to-option) :get))
) ;let

(let1 r ((right 1) :map (lambda (x) (+ x 1)))
  (check-true (r :right?))
  (check (r :get-or-else 0) => 2)
) ;let1

(let1 r ((right 1) :flat-map (lambda (x) (right (+ x 1))))
  (check-true (r :right?))
  (check (r :get-or-else 0) => 2)
) ;let1

(check-true ((left "error") :forall even?))
(check-true ((right 42) :forall even?))
(check-false ((right 43) :forall even?))
(check-false ((right "not-a-number") :forall number?))

(check-false ((left "error") :exists even?))
(check-true ((right 42) :exists even?))
(check-false ((right 43) :exists even?))
(check-false ((right "not-a-number") :exists number?))

(check-true (rich-vector :is-type-of (rich-vector :empty)))
(check-true (rich-vector :is-type-of (rich-vector #(1 2 3))))

(check-false (rich-vector :is-type-of #(1 2 3)))
(check-false (rich-vector :is-type-of 1))

(check (array :range 1 5) => ($ (vector 1 2 3 4)))
(check (array :range 1 5 2) => ($ (vector 1 3)))
(check (array :range 1 6 2) => ($ (vector 1 3 5)))
(check (array :range 5 1 -1) => ($ (vector 5 4 3 2)))

(check (array :range 5 1 1) => ($ (vector )))

(check-catch 'value-error (array :range 1 5 0))

(check (array :empty :empty?) => #t)
(check (array :empty :head-option) => (none))

(check-true (array :fill 0 #\a :empty?))

(check (array :fill 3 #\a) => ($ (vector #\a #\a #\a)))

(check ($ #() :length) => 0)
(check ($ #(1 2 3) :length) => 3)

(check ($ #() :size) => 0)
(check ($ #(1 2 3) :size) => 3)

(check ($ #(1 2 3) :apply 1) => 2)
(check ($ #(1 2 3) 1) => 2)

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :index-of 1) => 0)
  (check (vec :index-of 5) => 4)
  (check (vec :index-of 6) => -1)
) ;let

(let ((vec (array #(1 1 1 5 5))))
  (check (vec :last-index-of 1) => 2)
  (check (vec :last-index-of 5) => 4)
  (check (vec :last-index-of 6) => -1)
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check ((vec :find (lambda (x) (= x 3))) :get) => 3)
  (check ((vec :find (lambda (x) (> x 2))) :get) => 3)
  (check ((vec :find (lambda (x) (> x 10))) :empty?) => #t)
  (check ((vec :find even?) :get) => 2)
  (check ((vec :find (lambda (x) (< x 0))) :empty?) => #t)
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check ((vec :find-last even?) :get) => 4)  ; 最后一个偶数是4
  (check ((vec :find-last (@ > _ 3)) :get) => 5)  ; 最后一个大于3的元素是5
  (check ((vec :find-last (@ > _ 5)) :empty?) => #t)  ; 没有大于5的元素
  (check ((vec :find-last zero?) :empty?) => #t)  ; 没有0
  (check ((array :empty) :find-last even?) => (none))  ; 空向量返回none
) ;let

(check ($ (vector 1 2 3) :head) => 1)
(check-catch 'out-of-range (array :empty :head))
(check ($ (vector 1 2 3) :head-option) => (option 1))
(check (array :empty :head-option) => (none))

(check ($ (vector 1 2 3) :last) => 3)
(check-catch 'index-error (array :empty :last))
(check ($ (vector 1 2 3) :last-option) => (option 3))
(check (array :empty :last-option) => (none))

(let1 vec (array #(1 2 3 4 5))
  (check (vec :slice 0 2) => ($ #(1 2)))
  (check (vec :slice -1 2) => ($ #(1 2)))
  (check (vec :slice 2 -1) => ($ #()))
  (check (vec :slice 2 2) => ($ #()))
  (check (vec :slice 6 2) => ($ #()))
  (check (vec :slice -1 10) => ($ #(1 2 3 4 5)))
  (check (vec :slice 4 10) => ($ #(5)))
  (check (vec :slice 2 4) => ($ #(3 4)))
) ;let1

(check-true ($ (vector) :empty?))
(check-false ($ #(1 2 3) :empty?))

(check-true ($ #(1 2 3) :equals ($ #(1 2 3))))

(check ($ (vector ($ "中" 0) ($ "文" 0))) => ($ "中文" :to-vector))

(check-false (($ "中文" :to-rich-vector) :equals ($ "中" 0)))

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :forall (lambda (x) (> x 0))) => #t)
  (check (vec :forall (lambda (x) (> x 3))) => #f)
) ;let

(let ((empty-vec (array #())))
  (check (empty-vec :forall (lambda (x) (> x 0))) => #t)
) ;let

(let1 vec (rich-vector #(1 2 3))
  (check-true (vec :contains 1))
  (check-false (vec :contains 4))
) ;let1

(let1 vec (rich-vector #("/" "tmp" "/"))
  (check-true (vec :contains "tmp"))
  (check-true (vec :contains "/"))
  (check-false (vec :contains "tmpxx"))
) ;let1

(let1 vec (array #(1 2 3 4 5))
  (check (vec :map (lambda (x) (vector x x))) => #(#(1 1) #(2 2) #(3 3) #(4 4) #(5 5)))
) ;let1

;; 测试符号
(let1 vec (array #("a" ";" "." "?" "["))
  (check (vec :map (lambda (x) (vector x x))) => #(#("a" "a") #(";" ";") #("." ".") #("?" "?") #("[" "[")))
) ;let1

;; 混合测试
(let1 vec (array #("a" ";" "?" "[" -1 5))
  (check (vec :map (lambda (x) (vector x x))) => #(#("a" "a") #(";" ";") #("?" "?") #("[" "[") #(-1 -1) #(5 5)))
) ;let1

(let1 vec (array #(1 2 3 4 5))
  (check (vec :flat-map (lambda (x) (vector x x))) => #(1 1 2 2 3 3 4 4 5 5))
) ;let1

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check (vec :reverse :collect) => #(5 4 3 2 1))
) ;let

(let ((vec (rich-vector #(a b c d e))))
  (check (vec :reverse :collect) => #(e d c b a))
) ;let

(let ((vec (rich-vector #())))
  (check (vec :reverse :collect) => #())
) ;let

(let ((vec (rich-vector #("/" "tmp" "/" "tmp2"))))
  (check (vec :reverse :collect) => #("tmp2" "/" "tmp" "/"))
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :take -1 :collect) => #())
  (check (vec :take 0 :collect) => #())
  (check (vec :take 3 :collect) => #(1 2 3))
  (check (vec :take 5 :collect) => #(1 2 3 4 5))
  (check (vec :take 10 :collect) => #(1 2 3 4 5))
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :take-right -1 :collect) => #())
  (check (vec :take-right 0 :collect) => #())
  (check (vec :take-right 3 :collect) => #(3 4 5))
  (check (vec :take-right 5 :collect) => #(1 2 3 4 5))
  (check (vec :take-right 10 :collect) => #(1 2 3 4 5))
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :drop -1 :collect) => #(1 2 3 4 5))
  (check (vec :drop 0 :collect) => #(1 2 3 4 5))
  (check (vec :drop 3 :collect) => #(4 5))
  (check (vec :drop 5 :collect) => #())
  (check (vec :drop 10 :collect) => #())
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :drop-right -1 :collect) => #(1 2 3 4 5)) 
  (check (vec :drop-right 0 :collect) => #(1 2 3 4 5)) 
  (check (vec :drop-right 3 :collect) => #(1 2)) 
  (check (vec :drop-right 5 :collect) => #()) 
  (check (vec :drop-right 10 :collect) => #()) 
) ;let

(let ((vec (array #(1 2 3 4 5))) (empty-vec ($ #())))
  (check (vec :drop-while (@ < _ 3) :collect) => #(3 4 5))
  (check (vec :drop-while (@ > _ 3) :collect) => #(1 2 3 4 5))
  (check (vec :drop-while (@ < _ 3) :drop 1 :collect) => #(4 5))
  (check (empty-vec :drop-while (@ < _ 3) :drop 1 :collect) => #())
  (check (vec :drop-while (@ < _ 100) :collect) => #())
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :fold 0 +) => 15)
  (check (vec :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (vec :fold-right 0 +) => 15)
  (check (vec :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
) ;let

(check ($ #() :count) => 0)
(check ($ #() :count (@ > _ 2)) => 0)
(check ($ #(1 2 3 4 5) :count) => 5)
(check ($ #(1 2 3 4 5) :count (@ > _ 2)) => 3)

(let ((vec (rich-vector #(3 1 4 2 5))))
  (check (vec :sort-with <) => (array #(1 2 3 4 5)))
  (check (vec :sort-with >) => (array #(5 4 3 2 1)))
  (check (vec :sort-with < :collect) => #(1 2 3 4 5))
) ;let

(let ((vec (rich-vector #((2 . 1) (3 . 3) (1 . 3) (1 . 2) (3 . 2)))))
  (check (vec :sort-with (lambda (x y) (< (car x) (car y))))
         => (rich-vector #((1 . 3) (1 . 2) (2 . 1) (3 . 3) (3 . 2)))
  ) ;check
  (check (vec :sort-with (lambda (x y) (< (cdr x) (cdr y))))
         => (rich-vector #((2 . 1) (1 . 2) (3 . 2) (3 . 3) (1 . 3)))
  ) ;check
) ;let

;; 测试按绝对值排序
(check ($ #(-3 1 -2 4 0) :sort-by abs :collect) => #(0 1 -2 -3 4))
  
;; 测试按结构体字段排序
(let ((people ($ #((name . "Alice") (name . "Bob") (name . "Charlie")))))
  (check (people :sort-by (lambda (p) (string-length (cdr p))) :collect)
         => #((name . "Bob") (name . "Alice") (name . "Charlie"))
  ) ;check
) ;let
  
;; 测试空向量
(check ($ #() :sort-by identity :collect) => #())
  
;; 测试链式调用
(check ($ #(-3 1 -2 4 0) 
         :sort-by abs 
         :filter positive? 
         :collect)
       => #(1 4)
) ;check

(check  (($ #(1 2 3 4 5 6) :group-by (@ modulo _ 2)) :collect)
        =>  (hash-table 0 #(2 4 6) 1 #(1 3 5))
) ;check

(check  (($ #(1 2 3 4 5 6) :group-by (@ modulo _ 3)) :collect)
        =>  (hash-table 0 #(3 6) 1 #(1 4) 2 #(2 5))
) ;check

(check  (($ #(1 2 3 4 5 6 7) :group-by (@ modulo _ 3)) :collect)
        =>  (hash-table 0 #(3 6) 1 #(1 4 7) 2 #(2 5))
) ;check

(let ((result ($ #("apple" "banana" "cat" "dog") :group-by (@ string-length _))))
  (check (result :collect) 
    => (hash-table 3 #("cat" "dog") 5 #("apple") 6 #("banana"))
  ) ;check
) ;let

(check ($ #() :sliding 2) => #())
(check ($ #(1) :sliding 2) => #(#(1)))
(check ($ #(1 2) :sliding 2) => #(#(1 2)))
(check ($ #(1 2 3) :sliding 2) => #(#(1 2) #(2 3)))
(check ($ #(1 2 3 4 5) :sliding 3) => #(#(1 2 3) #(2 3 4) #(3 4 5)))
(check ($ #(1 2 3 4 5) :sliding 1) => #(#(1) #(2) #(3) #(4) #(5)))
(check ($ #(1 2 3) :sliding 3) => #(#(1 2 3)))
(check ($ #(1 2 3) :sliding 4) => #(#(1 2 3)))

;; Error cases for size
(check-catch 'value-error ($ #(1 2 3) :sliding 0))
(check-catch 'value-error ($ #(1 2 3) :sliding -1))
(check-catch 'type-error ($ #(1 2 3) :sliding 1.5))
(check-catch 'type-error ($ #(1 2 3) :sliding "a"))

;; Two-argument sliding
(check ($ #() :sliding 2 2) => #())
(check ($ #(1 2 3 4 5) :sliding 2 2) => #(#(1 2) #(3 4) #(5)))
(check ($ #(1 2 3 4 5 6) :sliding 2 3) => #(#(1 2) #(4 5)))
(check ($ #(1 2 3 4 5) :sliding 3 1) => #(#(1 2 3) #(2 3 4) #(3 4 5) #(4 5) #(5)))
(check ($ #(1 2 3 4) :sliding 2 2) => #(#(1 2) #(3 4)))
(check ($ #(1 2) :sliding 3 1) => #(#(1 2) #(2)))
(check ($ #(1 2 3 4 5) :sliding 3 2) => #(#(1 2 3) #(3 4 5) #(5)))
(check ($ #(1 2 3 4 5 6 7) :sliding 3 3) => #(#(1 2 3) #(4 5 6) #(7)))
(check ($ #(1 2 3 4 5) :sliding 5 1) => #(#(1 2 3 4 5) #(2 3 4 5) #(3 4 5) #(4 5) #(5)))
(check ($ #(1 2 3 4 5) :sliding 6 1) => #(#(1 2 3 4 5) #(2 3 4 5) #(3 4 5) #(4 5) #(5)))

;; Error cases for step (two-arg)
(check-catch 'value-error ($ #(1 2 3) :sliding 2 0))
(check-catch 'value-error ($ #(1 2 3) :sliding 2 -1))
(check-catch 'type-error ($ #(1 2 3) :sliding 2 1.5))
(check-catch 'type-error ($ #(1 2 3) :sliding 2 "a"))

(check  ($ #(a b c) :zip-with-index :collect)  
        => #((0 . a) (1 . b) (2 . c))
) ;check

(check  ($ #() :zip-with-index :collect) 
        => #()
) ;check

(check  ($ #(1 1 2 2 2 3 4 5 6 7) :zip-with-index :collect)
        => #((0 . 1) (1 . 1) (2 . 2) (3 . 2) (4 . 2) (5 . 3) (6 . 4) (7 . 5) (8 . 6) (9 . 7))
) ;check

(check  ($ #(a a b c b) :distinct :collect) 
        => #(a b c)
) ;check

(check  ($ #(1 1 1 2 2 3 3 3 3 5 5 5) :distinct :collect) 
        => #(1 2 3 5)
) ;check

(check  ($ #() :distinct :collect) 
        => #()
) ;check

(check ($ #(1 2 3 4) :reduce +) => 10)  ; 1 + 2 + 3 + 4 = 10
(check ($ #(5) :reduce *) => 5)         ; 单个元素直接返回
(check-catch 'value-error ($ #() :reduce +)) ; 空向量应该报错
(check ($ #(#(1 1) #(2 2) #(3 3) #(4 4) #(5 5))
         :map vector-length
         :reduce +)
  => 10
) ;check
(check ($ #(#(1 1) #(2 2) #(3 3) #(4 4) #(5 5))
         :map identity  ; 保持子向量不变
         :reduce vector-append)
       => #(1 1 2 2 3 3 4 4 5 5)
) ;check

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :index-where even?) => 1)
  (check (vec :index-where (@ > _ 3)) => 3)
  (check (vec :index-where (@ > _ 5)) => -1)
) ;let

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :last-index-where even?) => 3)
  (check (vec :last-index-where (@ > _ 3)) => 4)
  (check (vec :last-index-where (@ > _ 5)) => -1)
) ;let

(check ($ #(2 4 6 7 8 9) :take-while even?) => #(2 4 6))
(check ($ #(1 3 5 7) :take-while odd?) => #(1 3 5 7))
(check ($ #() :take-while even?) => #())
(check ($ #(1 2 3 4) :take-while even?) => #())
(check ($ #(0 0 0 1 0) :take-while zero?) => #(0 0 0))

(check ($ #(1 2 3 4 5) :max-by identity) => 5)
(check ($ #("apple" "banana" "pear") :max-by string-length) => "banana")
(check-catch 'value-error ($ #() :max-by identity))
(check-catch 'type-error ($ #(1 2 3) :max-by "not-a-function"))
(check-catch 'type-error ($ #(1 2 3) :max-by (lambda (x) "not-a-number")))

(check ($ #(1 2 3 4 5) :min-by identity) => 1)
(check ($ #("apple" "banana" "pear") :min-by string-length) => "pear")
(check-catch 'value-error ($ #() :min-by identity))
(check-catch 'type-error ($ #(1 2 3) :min-by "not-a-function"))
(check-catch 'type-error ($ #(1 2 3) :min-by (lambda (x) "not-a-number")))

(check ($ #() :max-by-option identity) => (none))

(check ($ #() :min-by-option identity) => (none))

(check (object->string ($ #(1 2 3))) => "#(1 2 3)")

(let ((vec ($ #("Hello" "World"))))
  (check (vec :to-string) => "#(\"Hello\" \"World\")")
) ;let

(let ((vec ($ #())))
  (check (vec :to-string) => "#()")
) ;let

(let ((vec ($ "test123 你好" :to-rich-vector)))
  (check (vec :to-string) => "#(#\\t #\\e #\\s #\\t #\\1 #\\2 #\\3 #\\space #\\你 #\\好)")
) ;let

(let1 v ($ #(1 2 3))
  (check (v :count) => 3)
  (check (v :count (cut > <> 1)) => 2)
  (check (v :make-string) => "123")
  (check (v :make-string " ") => "1 2 3")
  (check (v :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (v :make-string "[" ","))
  (check-catch 'type-error (v :make-string 123 "," "]"))
  (check-catch 'type-error (v :make-string "[" 123 "]"))
  (check-catch 'type-error (v :make-string "[" "," 123))
) ;let1

(check ($ #("a" "b" "c") :make-string) => "abc")

(let ((vec (rich-vector #(1 2 3))))
  (check (vec :to-list) => '(1 2 3))
) ;let

(let ((vec (rich-vector #(1 2 3))))
  (check (vec :to-rich-list) => (rich-list '(1 2 3)))
  (check ((vec :to-rich-list) :collect) => '(1 2 3))
) ;let

(let1 v ($ #(1 2 3))
  (v :set! 0 2)
  (check (v 0) => 2)
  (check-catch 'index-error (v -1))
  (check-catch 'index-error (v 3))
) ;let1

(check-catch 'index-error (array :empty :set! 0 1))

(check (rich-vector :empty :append #(1)) => #(1))
(check (rich-vector :empty :append ($ #(1))) => #(1))

(check ($ #(1) :append #()) => #(1))
(check ($ #(1) :append (rich-vector :empty)) => #(1))

(check ($ #(1) :append #(2 3)) => #(1 2 3))
(check ($ #(1) :append ($ #(2 3))) => #(1 2 3))
       
(check (rich-hash-table :empty) => ($ (hash-table)))
(check (rich-hash-table :empty :collect) => (hash-table))

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (check (ht :find (lambda (k v) (and (symbol? k) (even? v)))) => (option (cons 'b 2)))
  (check ((ht :find (lambda (k v) (> v 4))) :empty?) => #t)
) ;let1

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (check ((ht :get 'a) :get) => 1)
  (check ((ht :get 'd) :empty?) => #t)
) ;let1

(let1 ht1 ($ (hash-table 'a 1 'b 2 'c 3))
  (let1 ht2 (ht1 :remove 'b)
    (check-true  (ht1 :contains 'b))
    (check-false (ht2 :contains 'b))
    (check ((ht2 :get 'c) :get) => 3)
  ) ;let1
) ;let1

(let1 ht3 ($ (hash-table 'x 9 'y 8))
  (ht3 :remove! 'x)
  (check-false (ht3 :contains 'x))
  (check ((ht3 :get 'y) :get) => 8)
) ;let1

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (check-true (ht :contains 'a))
  (check-false (ht :contains 'd))
) ;let1

(let1 ht ($ (hash-table 'a 5 'b 8 'c 10 'd 12))
  (check (ht :forall (lambda (k v) (> v 4)))         => #t)  
  (check (ht :forall (lambda (k v) (< v 13)))        => #t)  
  (check (ht :forall (lambda (k v) (even? v)))       => #f)  
  
  (check (ht :forall (lambda (k v)                 
                       (and (symbol? k) (> v 4))))        => #t)  

  (check (ht :forall (lambda (k v)                 
                       (symbol? k)))                      => #t)  
  
  (check (ht :forall (lambda (k v) (eq? k v)))       => #f)  
) ;let1

(let1 ht-empty ($ (hash-table))
  (check (ht-empty :forall (lambda (k v) (string? v))) => #t)
) ;let1

(let1 ht-mixed ($ (hash-table 'id 10 'score 85 3.14 "pi"))
  (check (ht-mixed :forall (lambda (k v) (number? v))) => #f) 
  (check (ht-mixed :forall (lambda (k v) (and (integer? v) (even? v)))) => #f) 
) ;let1

(let1 ht-fail ($ (hash-table 'valid 42 'invalid "string"))
  (check (ht-fail :forall (lambda (k v) (number? v)))    => #f) 

  (check (ht-fail :forall (lambda (k v) 
                            (and (symbol? k) (number? v) (positive? v)))) => #f)
 ;check
) ;let1

;; nested hash table test
(let1 ht-nested ($ (hash-table 
                    'a ($ (hash-table 'x 10)) 
                    'b ($ (hash-table 'y 20))))
  (check (ht-nested :forall 
           (lambda (k sub-ht) 
             (sub-ht :forall (lambda (k v) (> v 9))))) => #t)
 ;check
) ;let1

(let ((ht ($ (hash-table 'a 1 'b "2" 'c 3))))
  (check (ht :exists (lambda (k v) (string? v))) => #t)
) ;let

(let ((ht ($ (hash-table "a" 1 'b 2 3 'c))))
  (check (ht :exists (lambda (k v) (number? k))) => #t)
) ;let

(let ((ht ($ (hash-table))))
  (check (ht :exists (lambda (k v) #t)) => #f)
) ;let

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (let1 r (ht :map (lambda (k v) (values k (+ v 1)))
              :collect)
    (check (r 'a) => 2)
    (check (r 'b) => 3)
    (check (r 'c) => 4)
  ) ;let1
) ;let1
      
(define ht 
  ($ (hash-table 'a 2 'b 5 'c 8 'd 10 'e 1 'f "test" 'g -2))
) ;define

(check (ht :count (lambda(k v) (and (number? v) (even? v)))) => 4)
(check (ht :count (lambda(k v) (and (number? v) (odd? v)))) => 2)

(let  ((ht ($ (hash-table 'x 10 'y 20 'z 30 'new 40)))     
       (sum 0))                                  
  (ht :for-each (lambda (k v) 
                  (set! sum (+ sum v)))             
  ) ;ht
  (check sum => 100)                             
) ;let

;; Empty hash table
(let ((ht ($ (make-hash-table)))                      
      (call-counter 0))                          
  
  (ht :for-each (lambda (k v) 
                  (set! call-counter (+ call-counter 1)))
  ) ;ht
  
  (check call-counter => 0)                      
) ;let

;; Nested hash tables
(let* ((inner ($ (hash-table 'x 100 'y 200)))      
       (outer ($ (hash-table 'a inner 'b 42)))     
       (total 0))                                  
  
  (outer :for-each 
    (lambda (k v)
      (if (case-class? v)
        (v  :for-each
            (lambda (k v)
              (set! total (+ total v))
            ) ;lambda
        ) ;v
        (set! total (+ total v))
      ) ;if
    ) ;lambda
  ) ;outer
  
  (check total => 342)                          
) ;let*

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (let1 r (ht :filter (lambda (k v) (even? v)) :collect)
    (check r => (hash-table 'b 2))
  ) ;let1
) ;let1

(check-report)

