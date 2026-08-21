# Goldfish Scheme Style

以 `goldfish/liii/project.scm:1-72` 为基准，后续所有 Scheme 以此为准。

## 1. 方括号

- 绑定列表与 `cond` 子句一律用 `[]`，不用 `()`。
  - `let` / `let*` / `let loop` / `let-values` 的绑定向量：`[var val]`
  - `cond` 的每个分支：`[test expr]`
- 其它位置（调用、参数列表、`lambda` 形参）保持 `()`。

```scheme
;; correct
(let loop ([dir (g_getcwd)])
  (cond [(or (not dir) (string=? dir "")) #f]
        [(g_isfile p) p]
        [else (let* ([p (path dir)]
                     [parent (path->string (path-parent p))])
                ...)]))

(let* ([lib-json (load-json-or-empty (find-lib-gfproject))]
       [local-json (load-json-or-empty (find-local-gfproject))])
  ...)
```

```scheme
;; wrong
(let loop ((dir (g_getcwd)))
  (cond ((or ...) #f)
        (else (let* ((p (path dir)) (parent ...)) ...))))
```

## 2. 对齐

- `let`/`let*` 的多个绑定按列对齐，第二列左对齐，`[` 对齐。
- `cond` 的分支按列对齐，`[` 对齐，`test` 与 `expr` 间保留固定间距使 `expr` 列对齐。
- `if` 的 `then`/`else` 分支缩进一级，不额外对齐。

```scheme
(let* ([p      (path dir)]
       [parent (path->string (path-parent p))])

(cond [(g_isfile p1) p1]
      [(g_isfile p2) p2]
      [else          #f])
```

## 3. `and` / `or` 替代 `if`

- ` (if test x #f)` / `(if test #f y)` / 单分支条件执行 优先用 `and` / `or`。
- 仅当需要显式 `else` 分支时用 `if`。

```scheme
;; correct
(and (g_isfile p) p)
(and (njson-object? local-tools)
     (not (njson-empty? local-tools))
     (let ([keys (vector->list (njson-keys local-tools))]) ...))

;; wrong
(if (g_isfile p) p #f)
(if (and (njson-object? x) (not (njson-empty? x))) (let (...) #f) #f)
(when (and ...) (let ...)) ; 用 and 替代 when
```

## 4. 闭括号

- 禁止 `)` / `]` 独占一行。
- 闭括号紧跟表达式，与表达式同行；多个闭括号连续收束在同一行。

```scheme
;; correct
(if (or (string=? parent dir) (string=? parent "") (string=? parent "/"))
  #f
  (loop parent)))])))

;; wrong
(if (or ...)
  #f
  (loop parent)
  )
)
; ) ;let
; ) ;if
```

- 不写 `) ;let` / `) ;if` 这类行尾注释。

## 5. 缩进与空行

- 2 空格缩进。
- `define-library` 顶层：`import`/`export`/`begin` 缩进 2；`define` 在 `begin` 内缩进 4；函数体缩进 6。
- 逻辑段落间空一行（如 `project-root` 与 `;; --- gfproject ...` 之间），`define` 间不额外空多行。

## 6. 其它

- `catch #t` 形式：`(catch #t (lambda () ...) (lambda args ...))`，`lambda` 参数写 `args`，`let` 绑定用 `[]`。
- `cond` 优先于嵌套 `if`；`let` 优先于 `let*` 当无依赖时。
- 字符串、字符字面量保持原样，不为对齐而拆行。
