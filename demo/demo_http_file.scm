(import (liii check)
        (liii http)
        (liii string)
        (liii json)
)

(define simpletex-api-key "你的api-key")

(if (or (= (string-length simpletex-api-key) 0)
        (string=? simpletex-api-key "你的api-key"))
    (display "SKIP SimpleTex OCR integration: fill simpletex-api-key in this test file to enable live API assertions.\n")
    (let* ((r (http-post "https://server.simpletex.cn/api/latex_ocr_turbo"
                :headers `(("token" . ,simpletex-api-key))
                :files '(("file" . ((file . "tests/resources/simpletex-formula-a2-b2.png")
                                    (filename . "simpletex-formula-a2-b2.png")
                                    (content-type . "image/png"))))))
           (json (string->json (r 'text))))
      (check (r 'status-code) => 200)
      (check (json-ref json "status") => #t)
      (check-true (string? (json-ref json "res" "latex")))
      (check-true (> (string-length (json-ref json "res" "latex")) 0))
    ) ;let*
) ;if