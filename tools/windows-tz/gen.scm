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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; Generate src/liii_date_time_windows_tz.h from the CLDR supplemental
;; data file windowsZones.xml
;; (https://github.com/unicode-org/cldr).
;;
;; Usage (from the repository root):
;;   bin/gf tools/windows-tz/gen.scm <path-to>/windowsZones.xml
;;
;; The output path defaults to src/liii_date_time_windows_tz.h; pass a
;; second argument to override it:
;;   bin/gf tools/windows-tz/gen.scm <xml> <output-header>

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii string)
  (liii sort)
) ;import

;; Return the value of attribute attr="..." in a mapZone line, or #f.

(define (attr-value line attr)
  (let* ((pat (string-append attr "=\"")) (pos (string-position pat line)))
    (and pos
      (let ((start (+ pos (string-length pat))))
        (let ((end (string-position "\"" line start)))
          (and end (substring line start end))
        ) ;let
      ) ;let
    ) ;and
  ) ;let*
) ;define

;; Collect (windows-name . iana-name) pairs from territory="001" entries.

(define (parse-zones path)
  (call-with-input-file path
    (lambda (port)
      (let loop
        ((line (read-line port)) (pairs '()))
        (if (eof-object? line)
          pairs
          (let ((other (attr-value line "other")) (type (attr-value line "type")))
            (if (and other type (string-contains line "territory=\"001\""))
              (loop (read-line port) (cons (cons other type) pairs))
              (loop (read-line port) pairs)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

(define (usage)
  (display "usage: bin/gf tools/windows-tz/gen.scm <windowsZones.xml> [output-header]"
  ) ;display
  (newline)
) ;define

(define (write-header path pairs)
  (call-with-output-file path
    (lambda (port)
      (define (out . parts)
        (for-each (lambda (p) (display p port)) parts)
        (newline port)
      ) ;define
      (out "//")
      (out "// Copyright (C) 2026 The Goldfish Scheme Authors")
      (out "//")
      (out "// Licensed under the Apache License, Version 2.0 (the \"License\");")
      (out "// you may not use this file except in compliance with the License.")
      (out "// You may obtain a copy of the License at")
      (out "//")
      (out "// http://www.apache.org/licenses/LICENSE-2.0")
      (out "//")
      (out "// Unless required by applicable law or agreed to in writing, software")
      (out "// distributed under the License is distributed on an \"AS IS\" BASIS, WITHOUT"
      ) ;out
      (out "// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the"
      ) ;out
      (out "// License for the specific language governing permissions and limitations"
      ) ;out
      (out "// under the License.")
      (out "//")
      (out "")
      (out "// Windows-to-IANA timezone mapping table, generated from the CLDR")
      (out "// supplemental data file common/supplemental/windowsZones.xml")
      (out "// (https://github.com/unicode-org/cldr).")
      (out "//")
      (out "// Data source copyright and license:")
      (out "//   Copyright (C) 1991-2013 Unicode, Inc.")
      (out "//   Unicode License - https://www.unicode.org/copyright.html")
      (out "//")
      (out "// Only the \"golden\" mappings (territory=\"001\") are kept: each Windows"
      ) ;out
      (out "// timezone name maps to its primary IANA timezone. Regenerate with:")
      (out "//   bin/gf tools/windows-tz/gen.scm <path-to>/windowsZones.xml")
      (out "// after updating the CLDR checkout.")
      (out "")
      (out "#ifndef LIII_DATE_TIME_WINDOWS_TZ_H")
      (out "#define LIII_DATE_TIME_WINDOWS_TZ_H")
      (out "")
      (out "#include <string.h>")
      (out "")
      (out "struct goldfish_windows_tz_entry {")
      (out "  const char* windows;")
      (out "  const char* iana;")
      (out "};")
      (out "")
      (out "// Sorted by `windows` for binary search. Do not edit by hand.")
      (out "static const struct goldfish_windows_tz_entry goldfish_windows_tz_map[]= {"
      ) ;out
      (for-each (lambda (p) (out "  {\"" (car p) "\", \"" (cdr p) "\"},")) pairs)
      (out "};")
      (out "")
      (out "// Return the IANA timezone name for a Windows timezone name, or NULL if")
      (out "// the name is not in the table.")
      (out "static const char*")
      (out "goldfish_windows_tz_to_iana (const char* windows_name) {")
      (out "  if (windows_name == NULL)")
      (out "    return NULL;")
      (out "  int lo= 0;")
      (out "  int hi= (int) (sizeof (goldfish_windows_tz_map) / sizeof (goldfish_windows_tz_map[0])) - 1;"
      ) ;out
      (out "  while (lo <= hi) {")
      (out "    int mid= (lo + hi) / 2;")
      (out "    int cmp = strcmp (windows_name, goldfish_windows_tz_map[mid].windows);"
      ) ;out
      (out "    if (cmp == 0)")
      (out "      return goldfish_windows_tz_map[mid].iana;")
      (out "    if (cmp < 0)")
      (out "      hi= mid - 1;")
      (out "    else")
      (out "      lo= mid + 1;")
      (out "  }")
      (out "  return NULL;")
      (out "}")
      (out "")
      (out "#endif /* LIII_DATE_TIME_WINDOWS_TZ_H */")
    ) ;lambda
  ) ;call-with-output-file
) ;define

(let ((args (cddr (command-line))))
  (if (null? args)
    (usage)
    (let ((xml-path (car args))
          (out-path (if (>= (length args) 2) (cadr args) "src/liii_date_time_windows_tz.h")
          ) ;out-path
         ) ;
      (let ((pairs
              (list-sort (lambda (a b) (string<? (car a) (car b))) (parse-zones xml-path))
            ) ;pairs
           ) ;
        (if (null? pairs)
          (begin
            (display "error: no territory=\"001\" entries found in ")
            (display xml-path)
            (newline)
            (exit 1)
          ) ;begin
          (begin
            (write-header out-path pairs)
            (display "wrote ")
            (display out-path)
            (display " (")
            (display (length pairs))
            (display " entries)")
            (newline)
          ) ;begin
        ) ;if
      ) ;let
    ) ;let
  ) ;if
) ;let
