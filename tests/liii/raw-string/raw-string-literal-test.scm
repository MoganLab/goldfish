;
; Copyright (C) 2026 The Goldfish Scheme Authors
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
        (liii raw-string))

(check-set-mode! 'report-failed)

(check #""""               => "")
(check #"" ""              => " ")
(check #""a""              => "a")
(check #""\""              => "\\")
(check #"-"""-"            => "\"")
(check #"-"\""-"           => "\\\"")
(check #"-"#"()""-"        => "#\"()\"")
(check #"-"#""a"""-"       =>  "#\"\"a\"\"")
(check #"-"ends with \""-" => "ends with \\\"")

(check
  #""multiline
string""
  => "multiline\nstring")

(check
  #""
    no whitespace stripping""
  => "\n    no whitespace stripping")

(check
  #""
    no whitespace stripping
  ""
  => "\n    no whitespace stripping\n  ")

(check
  #""
  注释 ;; comment
  ""
  => "\n  注释 ;; comment\n  ")

(check
  #"HTML"
<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>
  "HTML"
  => "\n<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>\n  ")

(check
  #"HTML"<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>
  "HTML"
  => "<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>\n  ")

(check
  #"HTML"<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>"HTML"
  => "<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>")


(check-report)
