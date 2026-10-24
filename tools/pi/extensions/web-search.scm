;;
;; Ported from pi-web-search (https://github.com/ttttmr/pi-web-search)
;; Original work Copyright (c) 2026 ttttmr
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base) (liii pi code extension) (liii pi web-search))

;; web-search 扩展：注册 web_search 工具（默认启用，见 PI_DEFAULT_TOOL_NAMES；
;; 实现位于 (liii pi web-search) 库，本文件只负责扩展注册）。

(define (web-search-extension api . opt-context)
  (register-tool! api
    (make-extension-tool-def "web_search"
      "Search the web using Gemini Google Search grounding."
      (pi-web-search-schema)
      pi-execute-web-search
      :snippet "Search the web for up-to-date information"
      :guidelines
      '("Use web_search when you need real-time information or external references.")
    ) ;make-extension-tool-def
  ) ;register-tool!
  "web-search-loaded"
) ;define

web-search-extension
