(define-library (liii sys)
  (export argv executable library-file stdin-tty? stdout-tty? stderr-tty?)
  (import (scheme process-context) (liii os))
  (begin
    (define (argv)
      (command-line)
    ) ;define

    (define (executable)
      (g_executable)
    ) ;define

    ;; 当前正在加载的 Scheme 源文件的绝对路径；无文件加载时返回 #f。
    ;; 库代码应在库体顶层调用并缓存结果，以定位与自身源码同目录的
    ;; 资源文件；运行期调用返回的是调用方正在加载的文件。
    (define (library-file)
      (g_library-file)
    ) ;define

    (define (stdin-tty?)
      (os-tty? 0)
    ) ;define

    (define (stdout-tty?)
      (os-tty? 1)
    ) ;define

    (define (stderr-tty?)
      (os-tty? 2)
    ) ;define

  ) ;begin
) ;define-library
