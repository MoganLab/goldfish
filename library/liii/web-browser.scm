;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii web-browser)
  (export web-browser-command web-browser-available? web-browser-open
    web-browser-open-new web-browser-open-new-tab
  ) ;export
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii error)
    (liii list)
    (liii os)
    (liii shutil)
    (liii string)
    (liii subprocess)
  ) ;import
  (begin

    ;; 安全校验 URL，防止参数注入与控制字符截断
    (define (%check-url url)
      (unless (string? url)
        (type-error "url must be a string")
      ) ;unless
      (let ((trimmed (string-trim url)))
        (when (string-null? trimmed)
          (value-error "url cannot be empty")
        ) ;when
        (when (string-starts? trimmed "-")
          (value-error (string-append "Invalid URL (leading dash disallowed): " url))
        ) ;when
        (string-for-each
          (lambda (c)
            (let ((code (char->integer c)))
              (when (or (< code 32) (= code 127))
                (value-error (string-append "Invalid URL (control characters disallowed): " url)
                ) ;value-error
              ) ;when
            ) ;let
          ) ;lambda
          url
        ) ;string-for-each
      ) ;let
    ) ;define

    ;; 替换 %s 或将 url 追加为参数列表的最后一项
    (define (%substitute-or-append tokens url)
      (let loop
        ((rest tokens) (acc '()) (replaced? #f))
        (cond
         ((null? rest) (if replaced? (reverse acc) (append (reverse acc) (list url))))
         ((string=? (car rest) "%s") (loop (cdr rest) (cons url acc) #t))
         (else (loop (cdr rest) (cons (car rest) acc) replaced?))
        ) ;cond
      ) ;let
    ) ;define

    ;; 判断字符是否为单引号或双引号
    (define (%quote-char? c)
      (let ((code (char->integer c)))
        (or (= code 34) (= code 39))
      ) ;let
    ) ;define

    ;; 将命令行字符串切分为 token 列表，支持单引号与双引号保护空格
    (define (%split-command-line str)
      (let ((len (string-length str)))
        (let loop
          ((i 0) (curr '()) (in-quote #f) (tokens '()))
          (if (>= i len)
            (let ((final-tokens
                    (if (null? curr) tokens (cons (list->string (reverse curr)) tokens))
                  ) ;final-tokens
                 ) ;
              (reverse final-tokens)
            ) ;let
            (let ((c (string-ref str i)))
              (cond ((and in-quote (char=? c in-quote)) (loop (+ i 1) curr #f tokens))
                    ((and (not in-quote) (%quote-char? c)) (loop (+ i 1) curr c tokens))
                    ((and (not in-quote) (char-whitespace? c))
                     (if (null? curr)
                       (loop (+ i 1) '() #f tokens)
                       (loop (+ i 1) '() #f (cons (list->string (reverse curr)) tokens))
                     ) ;if
                    ) ;
                    (else (loop (+ i 1) (cons c curr) in-quote tokens))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 解析 BROWSER 环境变量，支持以路径分隔符分隔的多候选
    (define (%parse-browser-env env-browser url)
      (let ((sep (string (pathsep))))
        (let loop
          ((choices (string-split env-browser sep)))
          (if (null? choices)
            #f
            (let* ((choice (string-trim (car choices)))
                   (raw-tokens (if (string-null? choice) '() (%split-command-line choice)))
                  ) ;
              (if (null? raw-tokens)
                (loop (cdr choices))
                (let* ((cmd (car raw-tokens))
                       (resolved-cmd (or (which cmd) (and (file-exists? cmd) cmd)))
                      ) ;
                  (if resolved-cmd
                    (let ((resolved-tokens (cons resolved-cmd (cdr raw-tokens))))
                      (%substitute-or-append resolved-tokens url)
                    ) ;let
                    (loop (cdr choices))
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 在候选列表中查找第一个存在的可执行程序
    (define (%find-first-browser candidates url)
      (let loop
        ((rest candidates))
        (if (null? rest)
          #f
          (let ((resolved (which (car rest))))
            (if resolved (list resolved url) (loop (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 解析指定 URL 在当前环境下的完整执行命令行
    ;; new = 0 为默认打开，new = 1 为新窗口，new = 2 为新标签页
    (define* (web-browser-command url (new 0))
      (%check-url url)
      (let ((env-browser (getenv "BROWSER")))
        (cond
         ((and (string? env-browser) (not (string-null? (string-trim env-browser))))
          (%parse-browser-env env-browser url)
         ) ;
         ((os-macos?)
          (let ((open-cmd (or (which "/usr/bin/open") (which "open"))))
            (if open-cmd (if (= new 1) (list open-cmd "-n" url) (list open-cmd url)) #f)
          ) ;let
         ) ;
         ((os-windows?)
          (let ((rundll (or (which "rundll32.exe") (which "rundll32"))))
            (if rundll
              (list rundll "url.dll,FileProtocolHandler" url)
              (%find-first-browser (list "msedge.exe" "chrome.exe" "firefox.exe") url)
            ) ;if
          ) ;let
         ) ;
         (else
           (let ((has-display? (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))))
             (if has-display?
               (cond ((which "xdg-open") => (lambda (p) (list p url)))
                     ((which "gio") => (lambda (p) (list p "open" url)))
                     (else (%find-first-browser (list "google-chrome" "chromium" "chromium-browser" "firefox")
                             url
                           ) ;%find-first-browser
                     ) ;else
               ) ;cond
               (if (getenv "TERM")
                 (%find-first-browser (list "www-browser" "w3m" "lynx" "links" "elinks") url)
                 #f
               ) ;if
             ) ;if
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define*

    ;; 判断当前系统和环境是否具备可调用的浏览器启动器
    (define (web-browser-available?)
      (and (web-browser-command "https://example.com") #t)
    ) ;define

    ;; 使用非阻塞子进程安全启动浏览器
    (define (%launch-browser cmdline)
      (guard (e (else #f))
        (let ((proc (process-spawn cmdline :stdin 'null :stdout 'discard :stderr 'discard)))
          (process? proc)
        ) ;let
      ) ;guard
    ) ;define

    ;; 使用默认浏览器打开 URL
    (define* (web-browser-open url (new 0))
      (let ((cmdline (web-browser-command url new)))
        (if cmdline (%launch-browser cmdline) #f)
      ) ;let
    ) ;define*

    ;; 尝试在新窗口中打开 URL
    (define (web-browser-open-new url)
      (web-browser-open url 1)
    ) ;define

    ;; 尝试在新标签页中打开 URL
    (define (web-browser-open-new-tab url)
      (web-browser-open url 2)
    ) ;define

  ) ;begin
) ;define-library
