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

(define-library (liii tui terminal key-map)
  (export %map-extended-key %map-standard-key)
  (import (scheme base) (liii tui keys parser))
  (begin

    (define (%map-extended-key code)
      (cond ((= code 72) 'up)
            ((= code 80) 'down)
            ((= code 75) 'left)
            ((= code 77) 'right)
            ((= code 71) 'home)
            ((= code 79) 'end)
            ((= code 73) 'page-up)
            ((= code 81) 'page-down)
            ((= code 83) 'delete)
            ((= code 82) 'insert)
            ((or (= code 15) (= code 353)) 'shift-tab)
            ((= code 115) 'ctrl-left)
            ((= code 116) 'ctrl-right)
            ((= code 119) 'ctrl-home)
            ((= code 117) 'ctrl-end)
            ((= code 141) 'ctrl-up)
            ((= code 145) 'ctrl-down)
            ((= code 147) 'ctrl-delete)
            ;; Alt 扩展键码（任务 1197）：沿用 DOS 101 键扩展码表的 Alt 行，
            ;; 与上方 Ctrl 行（115/116/117/141/145/147）同一张码表；
            ;; 由 src/liii_os.cpp 的 KEY_EVENT 分支在 Alt 按下时编码，
            ;; 供 Windows 控制台路径保留 Alt 修饰位（如 Alt+Up 撤回排队消息）。
            ;; Ctrl+Alt 组合沿用 Ctrl 码，由 %poll-console-key 依修饰状态另行处理
            ((= code 152) 'alt-up)
            ((= code 160) 'alt-down)
            ((= code 155) 'alt-left)
            ((= code 157) 'alt-right)
            ((= code 151) 'alt-home)
            ((= code 163) 'alt-end)
            ((= code 153) 'alt-page-up)
            ((= code 161) 'alt-page-down)
            ((= code 162) 'alt-insert)
            ((= code 164) 'alt-delete)
            ((and (>= code 59) (<= code 68))
             (string->symbol (string-append "f" (number->string (- code 58))))
            ) ;
            ((= code 133) 'f11)
            ((= code 134) 'f12)
            (else #f)
      ) ;cond
    ) ;define

    (define (%map-standard-key code)
      (cond ((= code 13) 'enter)
            ((= code 10) 'ctrl-j)
            ((= code 27) 'escape)
            ((or (= code 8) (= code 127)) 'backspace)
            ((= code 9) 'tab)
            ;; 裸字节 15 是真正的 Ctrl+O（pty/Windows 控制台层均如此）；
            ;; Shift+Tab 由 C++ 控制台层编码为 353（curses KEY_BTAB），
            ;; DOS conio 的 15 扩展码仅出现在 0/224 前缀之后（%map-extended-key）
            ((= code 353) 'shift-tab)
            ((= code 3) 'ctrl-c)
            ((= code 4) 'ctrl-d)
            ((and (>= code 32) (<= code 126)) (string (integer->char code)))
            (else (parse-key (string (integer->char code))))
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
