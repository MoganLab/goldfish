(import (liii check) (liii string))

;; string-tokenize
;; 将字符串按指定分隔符分割成多个子字符串（标记化）。
;;
;; 语法
;; ----
;; (string-tokenize str)
;; (string-tokenize str char)
;; (string-tokenize str char start)
;; (string-tokenize str char start end)
;;
;; 参数
;; ----
;; str : string?
;; 要标记化的源字符串。
;;
;; char : char? 可选
;; 用作分隔符的字符。省略时默认为空白字符(#\ )。
;;
;; start : integer? 可选
;; 搜索的起始位置索引（包含），默认为0。
;;
;; end : integer? 可选
;; token_type行为的结束位置索引（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; list
;; 返回一个字符串列表，包含由分隔符分割的所有非空子字符串。
;; 分隔符本身不包含在返回的子字符串中。
;; 如果字符串为空或只包含分隔符，返回空列表'()。
;;
;; 注意
;; ----
;; string-tokenize会从左到右扫描字符串，遇到分隔符时进行分割。
;; 连续的分隔符会被忽略，不会产生空字符串。
;; 对于空字符串输入，返回空列表'()。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char不是字符类型时
;; out-of-range 当start/end超出字符串索引范围时

(check (string-tokenize "a b c")
  =>
  '("a" "b" "c")
) ;check
(check (string-tokenize "a b c ")
  =>
  '("a" "b" "c" "")
) ;check
(check (string-tokenize " a b c")
  =>
  '("a" "b" "c")
) ;check
(check (string-tokenize "  a  b c  ")
  =>
  '("a" "b" "c" "")
) ;check
(check (string-tokenize "abc")
  =>
  '("abc")
) ;check
(check (string-tokenize "   ") => '(""))
(check (string-tokenize "") => '(""))

(check (string-tokenize "one,two,three" #\,)
  =>
  '("one" "two" "three")
) ;check
(check (string-tokenize "path/to/file" #\/)
  =>
  '("path" "to" "file")
) ;check
(check (string-tokenize "192.168.1.1" #\.)
  =>
  '("192" "168" "1" "1")
) ;check
(check (string-tokenize "hello:::world" #\:)
  =>
  '("hello" "world")
) ;check
(check (string-tokenize "test---case" #\-)
  =>
  '("test" "case")
) ;check

(check (string-tokenize "x") => '("x"))
(check (string-tokenize "x" #\x)
  =>
  '("")
) ;check
(check (string-tokenize "xx")
  =>
  '("xx")
) ;check
(check (string-tokenize "x x")
  =>
  '("x" "x")
) ;check
(check (string-tokenize "x x" #\x)
  =>
  '(" " "")
) ;check

(check (string-tokenize "hello\tworld\nscheme"
         #\tab
       ) ;string-tokenize
  =>
  '("hello" "world\nscheme")
) ;check
(check (string-tokenize "line1\nline2\nline3"
         #\newline
       ) ;string-tokenize
  =>
  '("line1" "line2" "line3")
) ;check
(check (string-tokenize "a|b|c|d" #\|)
  =>
  '("a" "b" "c" "d")
) ;check

(check (string-tokenize "The quick brown fox")
  =>
  '("The" "quick" "brown" "fox")
) ;check
(check (string-tokenize "multiple   spaces   here"
       ) ;string-tokenize
  =>
  '("multiple" "spaces" "here")
) ;check
(check (string-tokenize "comma,separated,values,test"
         #\,
       ) ;string-tokenize
  =>
  '("comma" "separated" "values" "test")
) ;check

(check (string-tokenize "hello world scheme"
         #\space
         6
       ) ;string-tokenize
  =>
  '("world" "scheme")
) ;check
(check (string-tokenize "hello world scheme"
         #\space
         0
         11
       ) ;string-tokenize
  =>
  '("hello" "world")
) ;check
(check (string-tokenize "hello world scheme"
         #\space
         6
         11
       ) ;string-tokenize
  =>
  '("world")
) ;check
(check (string-tokenize "a,b,c,d" #\, 2)
  =>
  '("b" "c" "d")
) ;check
(check (string-tokenize "a,b,c,d" #\, 0 3)
  =>
  '("a" "b")
) ;check

(check (string-tokenize "test string"
         #\space
         0
         4
       ) ;string-tokenize
  =>
  '("test")
) ;check
(check (string-tokenize "test string"
         #\space
         5
         11
       ) ;string-tokenize
  =>
  '("string")
) ;check
(check (string-tokenize "test string"
         #\space
         5
       ) ;string-tokenize
  =>
  '("string")
) ;check

(check (string-tokenize "123 456 789")
  =>
  '("123" "456" "789")
) ;check
(check (string-tokenize "file1.txt:file2.txt:file3.txt"
         #\:
       ) ;string-tokenize
  =>
  '("file1.txt" "file2.txt" "file3.txt")
) ;check
(check (string-tokenize "user@domain.com;user2@domain.com"
         #\;
       ) ;string-tokenize
  =>
  '("user@domain.com" "user2@domain.com")
) ;check

(check (string-tokenize "a,,b,,,c" #\,)
  =>
  '("a" "b" "c")
) ;check
(check (string-tokenize "::::" #\:)
  =>
  '("")
) ;check
(check (string-tokenize "a::b" #\:)
  =>
  '("a" "b")
) ;check
(check (string-tokenize "::a::" #\:)
  =>
  '("a" "")
) ;check

(check (string-tokenize "中文 测试 功能")
  =>
  '("中文" "测试" "功能")
) ;check

(check-catch 'wrong-type-arg
  (string-tokenize 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-tokenize "hello" "not-a-char")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-tokenize "hello" #\h 1.5)
) ;check-catch
(check-catch 'out-of-range
  (string-tokenize "hello" #\space -1)
) ;check-catch
(check-catch 'out-of-range
  (string-tokenize "hello" #\space 0 10)
) ;check-catch
(check-catch 'out-of-range
  (string-tokenize "" #\space 1)
) ;check-catch
(check-catch 'out-of-range
  (string-tokenize "test" #\space 5)
) ;check-catch

(check (let ((s "lisp scheme clojure"))
         (string-tokenize s)
       ) ;let
  =>
  '("lisp" "scheme" "clojure")
) ;check

(check (let ((data "2024-08-07 10:30:00"))
         (string-tokenize data #\- 0 10)
       ) ;let
  =>
  '("2024" "08" "07")
) ;check

(check-report)
