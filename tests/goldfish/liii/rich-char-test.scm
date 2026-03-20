;
; Copyright (C) 2025 The Goldfish Scheme Authors
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

(import (liii rich-char)
        (liii check)
        (liii lang)
) ;import

(check-true ((rich-char #x30) :equals (rich-char #x30)))
(check-false ((rich-char #x31) :equals (rich-char #x30)))

(check-true ((rich-char #x0) :ascii?))
(check-true ((rich-char #x7f) :ascii?))
(check-false ((rich-char #x8f) :ascii?))

(check-true ($ #\a :ascii?))
(check-true ($ #\Z :ascii?))

;; 数字字符
(check-true ($ #\3 :numeric?))
(check-true ($ #\4 :numeric?))
(check-true ($ #\5 :numeric?))
(check-true ($ #\0 :numeric?))

;; 非数字字符
(check-false ($ #\[ :numeric?))
(check-false ($ #\@ :numeric?))
(check-false ($ #\; :numeric?))
(check-false ($ #\P :numeric?))
(check-false ($ #\x :numeric?))

;; Unicode测试
(let ((char1 (rich-char 48))  ;; ASCII '0'
      (char2 (rich-char #xFF10))  ;; 全角 '０'
      (char3 (rich-char #x0660))  ;; 阿拉伯数字 '٠'
      (char4 (rich-char #x06F0))  ;; 扩展阿拉伯数字 '۰'
      (char5 (rich-char #x0966))  ;; 印度数字
      (char6 (rich-char #x09E6))  ;; 孟加拉数字
      (char7 (rich-char #x0A66))  ;; 古尔穆奇数字
      (char8 (rich-char #x0AE6))  ;; 古吉拉特数字
      (char9 (rich-char #x0B66))  ;; 奥里亚数字
      (char10 (rich-char #x0BE6))  ;; 泰米尔数字
      (char11 (rich-char #x0C66))  ;; 泰卢固数字
      (char12 (rich-char #x0CE6))  ;; 卡纳达数字 
      (char13 (rich-char #x0D66))  ;; 马拉雅拉姆数字
      (char14 (rich-char #x0E50))  ;; 泰文数字 '๐'
      (char15 (rich-char #x0ED0))  ;; 老挝数字
      (char16 (rich-char #x0F20))  ;; 藏文数字
      (char17 (rich-char #x1040))  ;; 缅甸数字 '၀'
      (char18 (rich-char #x17E0))  ;; 高棉数字 '០'
      (char19 (rich-char #x1810))  ;; 蒙古数字 '᠐'
      (char20 (rich-char 65)))  ;; ASCII 'A'
  
  (check (char1 :numeric?) => #t)  ;; ASCII 数字
  (check (char2 :numeric?) => #f)  ;; 全角数字
  (check (char3 :numeric?) => #f)  ;; 阿拉伯数字
  (check (char4 :numeric?) => #f)  ;; 扩展阿拉伯数字
  (check (char5 :numeric?) => #f)  ;; 印度数字
  (check (char6 :numeric?) => #f)  ;; 孟加拉数字
  (check (char7 :numeric?) => #f)  ;; 古尔穆奇数字
  (check (char8 :numeric?) => #f)  ;; 古吉拉特数字
  (check (char9 :numeric?) => #f)  ;; 奥里亚数字
  (check (char10 :numeric?) => #f)  ;; 泰米尔数字
  (check (char11 :numeric?) => #f)  ;; 泰卢固数字
  (check (char12 :numeric?) => #f)  ;; 卡纳达数字
  (check (char13 :numeric?) => #f)  ;; 马拉雅拉姆数字
  (check (char14 :numeric?) => #f)  ;; 泰文数字
  (check (char15 :numeric?) => #f)  ;; 老挝数字
  (check (char16 :numeric?) => #f)  ;; 藏文数字
  (check (char17 :numeric?) => #f)  ;; 缅甸数字
  (check (char18 :numeric?) => #f)  ;; 高棉数字
  (check (char19 :numeric?) => #f)  ;; 蒙古数字
  (check (char20 :numeric?) => #f)  ;; 非数字字符
) ;let
;; 大写字母
(check-true ($ #\A :upper?))
(check-true ($ #\Z :upper?))

;; 小写字母
(check-false ($ #\a :upper?))
(check-false ($ #\z :upper?))

;; 非字母字符
(check-false ($ #\0 :upper?))
(check-false ($ #\@ :upper?))  ;; @ 符号 (ASCII 64)
(check-false ($ #\[ :upper?))  ;; 左方括号 (ASCII 91)

;; 小写字母
(check-true ($ #\a :lower?))
(check-true ($ #\z :lower?))

;; 大写字母
(check-false ($ #\A :lower?))
(check-false ($ #\Z :lower?))

;; 非字母字符
(check-false ($ #\0 :lower?))
(check-false ($ #\` :lower?))  ;; 反引号 (ASCII 96)
(check-false ($ #\{ :lower?))  ;; 左花括号 (ASCII 123)

(let ((char1 (rich-char 48))  ;; ASCII '0'
      (char2 (rich-char #xFF10))  ;; 全角 '０'
      (char3 (rich-char #x0660))  ;; 阿拉伯数字 '٠'
      (char4 (rich-char #x06F0))  ;; 扩展阿拉伯数字 '۰'
      (char5 (rich-char #x0966))  ;; 印度数字
      (char6 (rich-char #x09E6))  ;; 孟加拉数字
      (char7 (rich-char #x0A66))  ;; 古尔穆奇数字
      (char8 (rich-char #x0AE6))  ;; 古吉拉特数字
      (char9 (rich-char #x0B66))  ;; 奥里亚数字
      (char10 (rich-char #x0BE6))  ;; 泰米尔数字
      (char11 (rich-char #x0C66))  ;; 泰卢固数字
      (char12 (rich-char #x0CE6))  ;; 卡纳达数字 
      (char13 (rich-char #x0D66))  ;; 马拉雅拉姆数字
      (char14 (rich-char #x0E50))  ;; 泰文数字 '๐'
      (char15 (rich-char #x0ED0))  ;; 老挝数字
      (char16 (rich-char #x0F20))  ;; 藏文数字
      (char17 (rich-char #x1040))  ;; 缅甸数字 '၀'
      (char18 (rich-char #x17E0))  ;; 高棉数字 '០'
      (char19 (rich-char #x1810))  ;; 蒙古数字 '᠐'
      (char20 (rich-char 65)))  ;; ASCII 'A'

  ;; 测试 %digit?
  (check (char1 :digit?) => #t)  ;; ASCII 数字
  (check (char2 :digit?) => #t)  ;; 全角数字
  (check (char3 :digit?) => #t)  ;; 阿拉伯数字
  (check (char4 :digit?) => #t)  ;; 扩展阿拉伯数字
  (check (char5 :digit?) => #t)  ;; 印度数字
  (check (char6 :digit?) => #t)  ;; 孟加拉数字
  (check (char7 :digit?) => #t)  ;; 古尔穆奇数字
  (check (char8 :digit?) => #t)  ;; 古吉拉特数字
  (check (char9 :digit?) => #t)  ;; 奥里亚数字
  (check (char10 :digit?) => #t)  ;; 泰米尔数字
  (check (char11 :digit?) => #t)  ;; 泰卢固数字
  (check (char12 :digit?) => #t)  ;; 卡纳达数字
  (check (char13 :digit?) => #t)  ;; 马拉雅拉姆数字
  (check (char14 :digit?) => #t)  ;; 泰文数字
  (check (char15 :digit?) => #t)  ;; 老挝数字
  (check (char16 :digit?) => #t)  ;; 藏文数字
  (check (char17 :digit?) => #t)  ;; 缅甸数字
  (check (char18 :digit?) => #t)  ;; 高棉数字
  (check (char19 :digit?) => #t)  ;; 蒙古数字
  (check (char20 :digit?) => #f)  ;; 非数字字符
) ;let

(check ($ #\a :to-upper) => #\A)
(check ($ #\z :to-upper) => #\Z)
(check ($ #\A :to-upper) => #\A)
(check ($ #\Z :to-upper) => #\Z)
(check ($ #\@ :to-upper) => #\@)

(check ($ #\Z :to-upper :to-lower) => #\z) ; chain

(check ($ #\A :to-lower) => #\a)
(check ($ #\Z :to-lower) => #\z)
(check ($ #\a :to-lower) => #\a)
(check ($ #\z :to-lower) => #\z)
(check ($ #\@ :to-lower) => #\@)

(check ($ #\z :to-lower :to-upper) => #\Z) ; chain

(check ($ #\space :to-string) => "#\\space")
(check ($ #\return :to-string) => "#\\return")

(check ($ #\a :to-string) => "#\\a")
(check ($ #\A :to-string) => "#\\A")

(check ((rich-char #xA3) :to-string) => "#\\£")

(check ((rich-char #x4E2D) :to-string) => "#\\中")
(check (object->string (rich-char #x4E2D)) => "#\\中")

(check ((rich-char #x1F600) :to-string) => "#\\😀")


(check ($ #\space :make-string) => " ")
(check ($ #\return :make-string) => (string #\return))

(check ($ #\a :make-string) => "a")
(check ($ #\A :make-string) => "A")

(check ((rich-char #xA3) :make-string) => "£")
(check ((rich-char #x4E2D) :make-string) => "中")
(check ((rich-char #x1F600) :make-string) => "😀")

(check-report)
