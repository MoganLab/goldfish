;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui clipboard native)
  (export native-clipboard-copy!
    native-clipboard-text
    native-clipboard-image-path
  ) ;export
  (import (scheme base)
    (scheme char)
    (scheme process-context)
    (liii os)
    (liii path)
    (liii string)
    (liii either)
    (liii subprocess)
  ) ;import
  (begin

    (define (%clipboard-command-output command)
      (catch #t
        (lambda ()
          (let ((result (run-either command :timeout 5 :stderr 'discard)))
            (if (either-right? result) (to-right result) #f)
          ) ;let
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define (%first-clipboard-text commands runner)
      (cond ((null? commands) #f)
            (else
              (let ((text (runner (car commands))))
                (if (and (string? text) (not (string-null? text)))
                  text
                  (%first-clipboard-text (cdr commands) runner)
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 测试可注入 runner：参数为 argv 列表，返回文本或 #f。
    (define* (native-clipboard-text (runner %clipboard-command-output))
      (catch #t
        (lambda ()
          (cond
           ((os-linux?)
            (%first-clipboard-text
              (append (if (getenv "WAYLAND_DISPLAY")
                        '(("wl-paste" "--no-newline" "--type" "text"))
                        '()
                      ) ;if
                (if (getenv "DISPLAY")
                  '(("xclip" "-selection" "clipboard" "-out")
                    ("xsel" "--clipboard" "--output"))
                  '()
                ) ;if
              ) ;append
              runner
            ) ;%first-clipboard-text
           ) ;
           ((os-macos?)
            (let ((t (runner '("pbpaste"))))
              (if (and (string? t) (not (string-null? t))) t #f)
            ) ;let
           ) ;
           ((os-windows?)
            (if (defined? 'g_os_get_clipboard_text)
              (let ((t (g_os_get_clipboard_text)))
                (if (and (string? t) (not (string-null? t))) t #f)
              ) ;let
              #f
            ) ;if
           ) ;
           (else #f)
          ) ;cond
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define*

    ;; 与读取方向相同：按当前图形环境选择命令，不依赖桌面工具是否存在。
    (define (%linux-clipboard-copy-commands)
      (append (if (getenv "WAYLAND_DISPLAY") '(("wl-copy")) '())
        (if (getenv "DISPLAY")
          '(("xclip" "-selection" "clipboard") ("xsel" "--clipboard" "--input"))
          '()
        ) ;if
      ) ;append
    ) ;define

    (define (%clipboard-command-input command text)
      (catch #t
        (lambda ()
          (zero? (run command :input text :timeout 5 :stdout 'discard :stderr 'discard))
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define (%first-clipboard-copy commands text runner)
      (cond ((null? commands) #f)
            (else
              (let ((copied? (catch #t (lambda () (runner (car commands) text)) (lambda _ #f))))
                (if copied? #t (%first-clipboard-copy (cdr commands) text runner))
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 测试可注入 runner：参数为 argv 列表与待复制文本，返回布尔值。
    ;; 生产执行器通过标准输入发送文本，避免 shell 转义及参数长度限制。
    (define* (native-clipboard-copy! text (runner %clipboard-command-input))
      (catch #t
        (lambda ()
          (cond ((os-windows?) (g_os_set_clipboard_text text))
                ((os-macos?) (runner '("pbcopy") text))
                ((os-linux?)
                 (%first-clipboard-copy (%linux-clipboard-copy-commands) text runner)
                ) ;
                (else #f)
          ) ;cond
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define*

    (define (%is-image-extension? p)
      (let ((ext (string-downcase (path-suffix (path p)))))
        (member ext '(".png" ".jpg" ".jpeg" ".gif" ".webp" ".bmp"))
      ) ;let
    ) ;define

    (define (%macos-save-clipboard-image target-path)
      (let* ((as-script (string-append "try\n"
                          "    set tempFile to POSIX file \"" target-path "\"\n"
                          "    set clipData to (the clipboard as «class PNGf»)\n"
                          "    set fileRef to (open for access tempFile with write permission)\n"
                          "    set eof fileRef to 0\n"
                          "    write clipData to fileRef\n"
                          "    close access fileRef\n" "    return \""
                          target-path "\"\n" "on error\n" "    try\n"
                          "        close access fileRef\n" "    end try\n"
                          "    try\n"
                          "        set p to POSIX path of (the clipboard as «class furl»)\n"
                          "        return \"furl:\" & p\n" "    on error\n"
                          "        return \"\"\n" "    end try\n" "end try"
                        ) ;string-append
             ) ;as-script
             (res (run-either (list "osascript" "-e" as-script) :timeout 5 :stderr 'discard))
            ) ;
        (if (either-right? res)
          (let ((out (string-trim-both (to-right res))))
            (cond
             ((string=? out target-path)
              (if
                (and (file-exists? target-path) (> (path-getsize (path target-path)) 0))
                target-path
                (begin
                  (when (file-exists? target-path)
                    (remove target-path)
                  ) ;when
                  #f
                ) ;begin
              ) ;if
             ) ;
             ((string-starts? out "furl:")
              (let ((fpath (substring out 5 (string-length out))))
                (if
                  (and (file-exists? fpath)
                    (%is-image-extension? fpath)
                    (not (path-dir? (path fpath)))
                  ) ;and
                  fpath
                  #f
                ) ;if
              ) ;let
             ) ;
             (else #f)
            ) ;cond
          ) ;let
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (%linux-save-clipboard-image target-path)
      (cond
       ((getenv "WAYLAND_DISPLAY")
        (let ((res (run-either (list "wl-paste" "--type" "image/png" "--no-newline")
                     :stdout  target-path
                     :timeout 5
                     :stderr  'discard
                   ) ;run-either
              ) ;res
             ) ;
          (if
            (and (either-right? res)
              (file-exists? target-path)
              (> (path-getsize (path target-path)) 0)
            ) ;and
            target-path
            (begin
              (when (file-exists? target-path)
                (remove target-path)
              ) ;when
              #f
            ) ;begin
          ) ;if
        ) ;let
       ) ;
       ((getenv "DISPLAY")
        (let ((res (run-either (list "xclip" "-selection" "clipboard" "-t" "image/png" "-o")
                     :stdout  target-path
                     :timeout 5
                     :stderr  'discard
                   ) ;run-either
              ) ;res
             ) ;
          (if
            (and (either-right? res)
              (file-exists? target-path)
              (> (path-getsize (path target-path)) 0)
            ) ;and
            target-path
            (begin
              (when (file-exists? target-path)
                (remove target-path)
              ) ;when
              #f
            ) ;begin
          ) ;if
        ) ;let
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (%windows-save-clipboard-image target-path)
      (let* ((ps-script (string-append "Add-Type -AssemblyName System.Windows.Forms; "
                          "Add-Type -AssemblyName System.Drawing; "
                          "$img = [System.Windows.Forms.Clipboard]::GetImage(); "
                          "if ($img) { $img.Save('" target-path
                          "', [System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'ok' } else { Write-Output 'empty' }"
                        ) ;string-append
             ) ;ps-script
             (res (run-either (list "powershell.exe" "-NoProfile" "-NonInteractive" "-Command" ps-script)
                    :timeout 5
                    :stderr  'discard
                  ) ;run-either
             ) ;res
            ) ;
        (if
          (and (either-right? res)
            (string-contains? (to-right res) "ok")
            (file-exists? target-path)
            (> (path-getsize (path target-path)) 0)
          ) ;and
          target-path
          (begin
            (when (file-exists? target-path)
              (remove target-path)
            ) ;when
            #f
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define* (native-clipboard-image-path (runner #f))
      (catch #t
        (lambda ()
          (if (procedure? runner)
            (runner)
            (let ((tmp
                    (path->string
                      (path-join (path-temp-dir)
                        (string-append "pi-clipboard-" (number->string (current-jiffy)) ".png")
                      ) ;path-join
                    ) ;path->string
                  ) ;tmp
                 ) ;
              (cond ((os-macos?) (%macos-save-clipboard-image tmp))
                    ((os-linux?) (%linux-save-clipboard-image tmp))
                    ((os-windows?) (%windows-save-clipboard-image tmp))
                    (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define*

  ) ;begin
) ;define-library
