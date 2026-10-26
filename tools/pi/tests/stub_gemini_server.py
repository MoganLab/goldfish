import http.server
import socketserver
import sys
import json
import threading

class StubHandler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        len_h = int(self.headers.get("content-length", 0))
        body = self.rfile.read(len_h) if len_h > 0 else b""
        req_json = {}
        if body:
            try:
                req_json = json.loads(body.decode("utf-8"))
            except:
                pass
        
        path = self.path
        if "/v1/messages" in path:
            if "error-400" in path or "/error-400" in self.headers.get("Host", "") or "/error-400" in path:
                self.send_response(400)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(b'{"type":"error","error":{"type":"invalid_request_error","message":"model not found"}}')
                return

            if "429-then-ok" in path:
                server = self.server
                server.request_count = getattr(server, "request_count", 0) + 1
                if server.request_count < 3:
                    self.send_response(429)
                    self.send_header("Content-Type", "application/json")
                    self.end_headers()
                    self.wfile.write(b'{"type":"error","error":{"type":"rate_limit_error","message":"Rate limit exceeded"}}')
                    return

            if "/error-500" in path:
                self.send_response(500)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(b'{"type":"error","error":{"type":"api_error","message":"Internal Server Error"}}')
                return

            if "/max-tokens" in path:
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({
                    "id": "msg_max_tokens",
                    "type": "message",
                    "role": "assistant",
                    "content": [{"type": "text", "text": "Truncated"}],
                    "stop_reason": "max_tokens",
                    "usage": {"input_tokens": 10, "output_tokens": 5}
                }).encode("utf-8"))
                return

            is_stream = req_json.get("stream", False) or "/stream" in path
            if "/tool-call" in path:
                has_tool_result = any(
                    any(b.get("type") == "tool_result" for b in m.get("content", []))
                    if isinstance(m.get("content"), list) else False
                    for m in req_json.get("messages", [])
                )
                if is_stream:
                    self.send_response(200)
                    self.send_header("Content-Type", "text/event-stream")
                    self.send_header("Cache-Control", "no-cache")
                    self.end_headers()
                    if has_tool_result:
                        evs = [
                            ("message_start", {"type": "message_start", "message": {"id": "msg_done", "type": "message", "role": "assistant", "usage": {"input_tokens": 25, "output_tokens": 0}}}),
                            ("content_block_start", {"type": "content_block_start", "index": 0, "content_block": {"type": "text", "text": ""}}),
                            ("content_block_delta", {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "Tool executed successfully!"}}),
                            ("content_block_stop", {"type": "content_block_stop", "index": 0}),
                            ("message_delta", {"type": "message_delta", "delta": {"stop_reason": "end_turn"}, "usage": {"output_tokens": 5}}),
                            ("message_stop", {"type": "message_stop"})
                        ]
                    else:
                        evs = [
                            ("message_start", {"type": "message_start", "message": {"id": "msg_tc", "type": "message", "role": "assistant", "usage": {"input_tokens": 20, "output_tokens": 0}}}),
                            ("content_block_start", {"type": "content_block_start", "index": 0, "content_block": {"type": "tool_use", "id": "ant_call_1", "name": "bash", "input": {}}}),
                            ("content_block_delta", {"type": "content_block_delta", "index": 0, "delta": {"type": "input_json_delta", "partial_json": '{"command":"echo test"}'}}),
                            ("content_block_stop", {"type": "content_block_stop", "index": 0}),
                            ("message_delta", {"type": "message_delta", "delta": {"stop_reason": "tool_use"}, "usage": {"output_tokens": 10}}),
                            ("message_stop", {"type": "message_stop"})
                        ]
                    for ev_name, ev_data in evs:
                        self.wfile.write(f"event: {ev_name}\ndata: {json.dumps(ev_data)}\n\n".encode("utf-8"))
                        self.wfile.flush()
                    return

                if has_tool_result:
                    resp = {
                        "id": "msg_tool_done",
                        "type": "message",
                        "role": "assistant",
                        "content": [{"type": "text", "text": "Tool executed successfully!"}],
                        "stop_reason": "end_turn",
                        "usage": {"input_tokens": 25, "output_tokens": 5}
                    }
                else:
                    resp = {
                        "id": "msg_tool_1",
                        "type": "message",
                        "role": "assistant",
                        "content": [
                            {"type": "tool_use", "id": "ant_call_1", "name": "bash", "input": {"command": "echo test"}}
                        ],
                        "stop_reason": "tool_use",
                        "usage": {"input_tokens": 20, "output_tokens": 10}
                    }
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps(resp).encode("utf-8"))
                return

            if "/compat-gateway" in path:
                if is_stream:
                    self.send_response(200)
                    self.send_header("Content-Type", "text/event-stream")
                    self.send_header("Cache-Control", "no-cache")
                    self.end_headers()
                    evs = [
                        ("message_start", {"type": "message_start", "message": {"id": "msg_cg1", "type": "message", "role": "assistant", "model": "glm-5-turbo", "content": [], "stop_reason": None, "usage": {"input_tokens": 0, "output_tokens": 0}}}),
                        ("ping", {"type": "ping"}),
                        ("content_block_start", {"type": "content_block_start", "index": 0, "content_block": {"type": "thinking", "thinking": "", "signature": ""}}),
                        ("content_block_delta", {"type": "content_block_delta", "index": 0, "delta": {"type": "thinking_delta", "thinking": "Thinking process..."}}),
                        ("content_block_delta", {"type": "content_block_delta", "index": 0, "delta": {"type": "signature_delta", "signature": "sig_abc"}}),
                        ("content_block_stop", {"type": "content_block_stop", "index": 0}),
                        ("content_block_start", {"type": "content_block_start", "index": 1, "content_block": {"type": "text", "text": ""}}),
                        ("content_block_delta", {"type": "content_block_delta", "index": 1, "delta": {"type": "text_delta", "text": "Hello from compatible gateway!"}}),
                        ("content_block_stop", {"type": "content_block_stop", "index": 1}),
                        ("message_delta", {"type": "message_delta", "delta": {"stop_reason": "end_turn", "stop_sequence": None}, "usage": {"input_tokens": 15, "output_tokens": 8, "cache_read_input_tokens": 4}}),
                        ("message_stop", {"type": "message_stop"})
                    ]
                    for ev_name, ev_data in evs:
                        self.wfile.write(f"event: {ev_name}\ndata: {json.dumps(ev_data)}\n\n".encode("utf-8"))
                        self.wfile.flush()
                    return
                else:
                    resp = {
                        "id": "msg_cg_nonstream",
                        "type": "message",
                        "role": "assistant",
                        "model": "glm-5-turbo",
                        "content": [
                            {"type": "thinking", "thinking": "Thinking non-stream", "signature": "sig_1"},
                            {"type": "text", "text": "Hello from compatible gateway non-stream!"}
                        ],
                        "stop_reason": "end_turn",
                        "usage": {"input_tokens": 15, "output_tokens": 8, "cache_read_input_tokens": 4}
                    }
                    self.send_response(200)
                    self.send_header("Content-Type", "application/json")
                    self.end_headers()
                    self.wfile.write(json.dumps(resp).encode("utf-8"))
                    return

            if is_stream:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                evs = [
                    ("message_start", {"type": "message_start", "message": {"id": "msg_s1", "type": "message", "role": "assistant", "usage": {"input_tokens": 12, "output_tokens": 0}}}),
                    ("content_block_start", {"type": "content_block_start", "index": 0, "content_block": {"type": "text", "text": ""}}),
                    ("content_block_delta", {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "Hello "}}),
                    ("content_block_delta", {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "from Claude stream!"}}),
                    ("content_block_stop", {"type": "content_block_stop", "index": 0}),
                    ("message_delta", {"type": "message_delta", "delta": {"stop_reason": "end_turn"}, "usage": {"output_tokens": 7}}),
                    ("message_stop", {"type": "message_stop"})
                ]
                for ev_name, ev_data in evs:
                    self.wfile.write(f"event: {ev_name}\ndata: {json.dumps(ev_data)}\n\n".encode("utf-8"))
                    self.wfile.flush()
                return

            resp = {
                "id": "msg_ant_1",
                "type": "message",
                "role": "assistant",
                "content": [{"type": "text", "text": "Hello from Claude stub!"}],
                "stop_reason": "end_turn",
                "usage": {"input_tokens": 15, "output_tokens": 5},
                "echo": {
                    "apiKey": self.headers.get("x-api-key"),
                    "anthropicVersion": self.headers.get("anthropic-version"),
                    "body": req_json
                }
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/chat/completions" in path:
            # 任务 033：kimi-coding（OpenAI 兼容）multikey 轮换 e2e。
            # 场景由 base URL 路径标记（如 /mk-kimi-403）区分，key 取自 Authorization。
            auth = self.headers.get("Authorization") or ""
            api_key = auth[7:] if auth.startswith("Bearer ") else ""
            server = self.server
            server.mk_kimi_keys_seen = getattr(server, "mk_kimi_keys_seen", [])
            server.mk_kimi_keys_seen.append(api_key)
            is_sse = bool(req_json.get("stream"))
            status_1 = None
            if "/mk-kimi-all-403" in path:
                status_1 = 403
            elif api_key.startswith("mk-kimi-key-1-") and "/mk-kimi-plain" not in path:
                if "/mk-kimi-403" in path or "/mk-kimi-stream-403" in path:
                    status_1 = 403
                elif "/mk-kimi-429" in path or "/mk-kimi-stream-429" in path:
                    status_1 = 429
            if status_1 is not None:
                self.send_response(status_1)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({
                    "error": {
                        "message": "key rejected in kimi stub",
                        "type": "rate_limit_error" if status_1 == 429 else "permission_error",
                        "code": status_1
                    }
                }).encode("utf-8"))
                return
            prefix = "PlainOK:" if "/mk-kimi-plain" in path else "RotatedOK:"
            text = prefix + api_key + " seen=" + str(len(server.mk_kimi_keys_seen))
            if is_sse:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                first = {
                    "id": "chatcmpl-mk",
                    "object": "chat.completion.chunk",
                    "choices": [
                        {"index": 0, "delta": {"role": "assistant", "content": text}, "finish_reason": None}
                    ]
                }
                last = {
                    "id": "chatcmpl-mk",
                    "object": "chat.completion.chunk",
                    "choices": [{"index": 0, "delta": {}, "finish_reason": "stop"}],
                    "usage": {"prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15}
                }
                self.wfile.write(("data: " + json.dumps(first) + "\n\n").encode("utf-8"))
                self.wfile.write(("data: " + json.dumps(last) + "\n\n").encode("utf-8"))
                self.wfile.write(b"data: [DONE]\n\n")
                self.wfile.flush()
                return
            resp = {
                "id": "chatcmpl-mk",
                "object": "chat.completion",
                "model": "kimi-mk",
                "choices": [
                    {"index": 0,
                     "message": {"role": "assistant", "content": text},
                     "finish_reason": "stop"}
                ],
                "usage": {"prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15}
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/multikey-" in path:
            # 任务 033：multikey 轮换 e2e。按 x-goog-api-key 头区分 key：
            #   mk-gem-key-1-* → /multikey-403 回 403，/multikey-429 回 429
            #   mk-gem-key-2-* → 正常响应（文本含 RotatedOK 与所用 key）
            #   其他 key       → 回显 key（无 pool 兼容用例）
            # /multikey-all-403 对任意 key 回 403（pool 耗尽用例）
            # 路径含 /multikey-stream-* 时以 SSE 形态应答
            is_sse = "alt=sse" in path or ":streamGenerateContent" in path
            api_key_hdr = self.headers.get("x-goog-api-key") or ""
            server = self.server
            server.mk_keys_seen = getattr(server, "mk_keys_seen", [])
            server.mk_keys_seen.append(api_key_hdr)
            if "/multikey-sticky" in path:
                server.mk_sticky_keys_seen = getattr(server, "mk_sticky_keys_seen", [])
                server.mk_sticky_keys_seen.append(api_key_hdr)
            status_1 = None
            if "/multikey-all-403" in path:
                status_1 = 403
            elif api_key_hdr.startswith("mk-gem-key-1-") and "plain" not in path:
                if "/multikey-403" in path or "/multikey-stream-403" in path:
                    status_1 = 403
                elif "/multikey-429" in path or "/multikey-stream-429" in path:
                    status_1 = 429
            elif not api_key_hdr.startswith("mk-gem-key-2-") or "plain" in path:
                text = "PlainOK:" + api_key_hdr + " seen=" + str(len(server.mk_keys_seen))
                resp = {
                    "candidates": [
                        {
                            "content": {"role": "model", "parts": [{"text": text}]},
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 10, "candidatesTokenCount": 5, "totalTokenCount": 15}
                }
                if is_sse:
                    self.send_response(200)
                    self.send_header("Content-Type", "text/event-stream")
                    self.send_header("Cache-Control", "no-cache")
                    self.end_headers()
                    self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                    self.wfile.flush()
                    return
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps(resp).encode("utf-8"))
                return
            if status_1 is not None:
                self.send_response(status_1)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({
                    "error": {"code": status_1,
                              "message": "key rejected in stub",
                              "status": "RESOURCE_EXHAUSTED" if status_1 == 429 else "PERMISSION_DENIED"}
                }).encode("utf-8"))
                return
            if "/multikey-sticky" in path:
                contents = req_json.get("contents", [])
                has_fn_response = any(
                    "functionResponse" in p
                    for c in contents
                    for p in c.get("parts", [])
                    if isinstance(p, dict)
                )
                if has_fn_response:
                    text = "StickyOK:" + ",".join(server.mk_sticky_keys_seen)
                    resp = {
                        "candidates": [
                            {
                                "content": {"role": "model", "parts": [{"text": text}]},
                                "finishReason": "STOP"
                            }
                        ],
                        "usageMetadata": {"promptTokenCount": 40, "candidatesTokenCount": 10, "totalTokenCount": 50}
                    }
                else:
                    resp = {
                        "candidates": [
                            {
                                "content": {
                                    "role": "model",
                                    "parts": [
                                        {
                                            "functionCall": {
                                                "name": "bash",
                                                "args": {"command": "echo sticky"},
                                                "id": "sticky_call_1"
                                            }
                                        }
                                    ]
                                },
                                "finishReason": "STOP"
                            }
                        ],
                        "usageMetadata": {"promptTokenCount": 20, "candidatesTokenCount": 10, "totalTokenCount": 30}
                    }
                if is_sse:
                    self.send_response(200)
                    self.send_header("Content-Type", "text/event-stream")
                    self.send_header("Cache-Control", "no-cache")
                    self.end_headers()
                    self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                    self.wfile.flush()
                    return
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps(resp).encode("utf-8"))
                return
            text = "RotatedOK:" + api_key_hdr + " seen=" + str(len(server.mk_keys_seen))
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": [{"text": text}]},
                        "finishReason": "STOP"
                    }
                ],
                "usageMetadata": {"promptTokenCount": 10, "candidatesTokenCount": 5, "totalTokenCount": 15}
            }
            if is_sse:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/429-then-ok" in path:
            server = self.server
            server.request_count = getattr(server, "request_count", 0) + 1
            if server.request_count < 3:
                self.send_response(429)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(b'{"error":{"code":429,"message":"Rate limit exceeded","status":"RESOURCE_EXHAUSTED"}}')
                return

        if "/429-5-then-ok" in path:
            # 前 5 次请求返回 429，之后正常响应；独立计数器
            server = self.server
            server.count_429_5 = getattr(server, "count_429_5", 0) + 1
            if server.count_429_5 <= 5:
                self.send_response(429)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(b'{"error":{"code":429,"message":"Rate limit exceeded","status":"RESOURCE_EXHAUSTED"}}')
                return

        if "/429-always" in path:
            self.send_response(429)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"error":{"code":429,"message":"Rate limit exceeded","status":"RESOURCE_EXHAUSTED"}}')
            return

        if "/error-500" in path:
            self.send_response(500)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"error":{"code":500,"message":"Internal Server Error"}}')
            return

        if "/max-tokens" in path:
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": [{"text": "Partial cut"}]},
                        "finishReason": "MAX_TOKENS"
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 10,
                    "candidatesTokenCount": 5,
                    "totalTokenCount": 15
                }
            }
            if "alt=sse" in path or ":streamGenerateContent" in path:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/400" in path:
            err_resp = {
                "error": {
                    "code": 400,
                    "message": "Invalid value at 'system_instruction': Content object expected",
                    "status": "INVALID_ARGUMENT"
                }
            }
            body = json.dumps(err_resp).encode("utf-8")
            self.send_response(400)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            try:
                self.wfile.write(body)
                self.wfile.flush()
            except (BrokenPipeError, ConnectionResetError):
                pass
            return

        if "/safety" in path:
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": []},
                        "finishReason": "SAFETY"
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 5,
                    "candidatesTokenCount": 0,
                    "totalTokenCount": 5
                }
            }
            if "alt=sse" in path or ":streamGenerateContent" in path:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        is_search_grounding = False
        for t in req_json.get("tools", []):
            if isinstance(t, dict) and ("googleSearch" in t or "google_search" in t):
                is_search_grounding = True
                break

        if "/web-search-basic" in path or "/models/web-search-basic" in path or (is_search_grounding and "/web-search-" not in path):
            resp = {
                "candidates": [
                    {
                        "content": {
                            "role": "model",
                            "parts": [{"text": "Goldfish Scheme is a minimalist Scheme implementation."}]
                        },
                        "finishReason": "STOP",
                        "groundingMetadata": {
                            "webSearchQueries": ["goldfish scheme"],
                            "groundingChunks": [
                                {
                                    "web": {
                                        "title": "Goldfish Official",
                                        "uri": "https://goldfish.liii.pro"
                                    }
                                },
                                {
                                    "web": {
                                        "title": "GitHub Goldfish",
                                        "uri": "https://github.com/XmacsLabs/goldfish"
                                    }
                                }
                            ],
                            "groundingSupports": [
                                {
                                    "segment": {
                                        "startIndex": 0,
                                        "endIndex": 50,
                                        "text": "Goldfish Scheme is a minimalist Scheme implementation."
                                    },
                                    "groundingChunkIndices": [0]
                                }
                            ]
                        }
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 20,
                    "candidatesTokenCount": 15,
                    "totalTokenCount": 35
                }
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/web-search-urls" in path or "/models/web-search-urls" in path:
            user_text = ""
            for c in req_json.get("contents", []):
                for p in c.get("parts", []):
                    user_text += p.get("text", "")
            has_urls_prompt = "Also analyze these URLs:" in user_text
            resp = {
                "candidates": [
                    {
                        "content": {
                            "role": "model",
                            "parts": [{"text": f"Analyzed query and URLs (prompt_has_urls={has_urls_prompt})."}]
                        },
                        "finishReason": "STOP",
                        "groundingMetadata": {
                            "webSearchQueries": ["custom query"],
                            "groundingChunks": [
                                {
                                    "web": {
                                        "title": "URL 1",
                                        "uri": "https://example.com/1"
                                    }
                                }
                            ],
                            "groundingSupports": [
                                {
                                    "segment": {
                                        "startIndex": 0,
                                        "endIndex": 20,
                                        "text": "Analyzed query"
                                    },
                                    "groundingChunkIndices": [0]
                                }
                            ]
                        }
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 30,
                    "candidatesTokenCount": 10,
                    "totalTokenCount": 40
                }
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/web-search-401" in path or "/models/web-search-401" in path:
            self.send_response(401)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"error":{"code":401,"message":"API key not valid. Please pass a valid API key.","status":"UNAUTHENTICATED"}}')
            return

        if "/web-search-403" in path or "/models/web-search-403" in path:
            self.send_response(403)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"error":{"code":403,"message":"Permission denied or quota exceeded","status":"PERMISSION_DENIED"}}')
            return

        if "/web-search-non-json" in path or "/models/web-search-non-json" in path:
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.end_headers()
            self.wfile.write(b'<html><body>502 Bad Gateway</body></html>')
            return

        if "/web-search-empty-candidates" in path or "/models/web-search-empty-candidates" in path:
            resp = {
                "candidates": []
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/web-search-hang" in path or "/models/web-search-hang" in path:
            import time
            time.sleep(3)
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"candidates":[]}')
            return

        if "/function-call" in path:
            resp = {
                "candidates": [
                    {
                        "content": {
                            "role": "model",
                            "parts": [
                                {
                                    "functionCall": {
                                        "name": "bash",
                                        "args": {"command": "ls -la"},
                                        "id": "bash_call_1"
                                    }
                                }
                            ]
                        },
                        "finishReason": "STOP"
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 20,
                    "candidatesTokenCount": 10,
                    "totalTokenCount": 30
                }
            }
            if "alt=sse" in path or ":streamGenerateContent" in path:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/agent-web-search" in path or "/models/agent-web-search" in path:
            contents = req_json.get("contents", [])
            fn_responses = []
            for c in contents:
                for p in c.get("parts", []):
                    if "functionResponse" in p:
                        fn_responses.append(p["functionResponse"])
            if len(fn_responses) == 0:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [
                                    {
                                        "functionCall": {
                                            "name": "web_search",
                                            "args": {"query": "Goldfish Scheme"},
                                            "id": "search_call_1"
                                        }
                                    }
                                ]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 20, "candidatesTokenCount": 10, "totalTokenCount": 30}
                }
            else:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [{"text": "Goldfish Scheme is a minimalist Scheme implementation.\n\n## Sources\n[1] Goldfish Official - https://goldfish.liii.pro"}]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 60, "candidatesTokenCount": 20, "totalTokenCount": 80}
                }
            if "alt=sse" in path or ":streamGenerateContent" in path:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/multi-turn" in path:
            contents = req_json.get("contents", [])
            fn_responses = []
            for c in contents:
                for p in c.get("parts", []):
                    if "functionResponse" in p:
                        fn_responses.append(p["functionResponse"])
            if len(fn_responses) == 0:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [
                                    {
                                        "functionCall": {
                                            "name": "bash",
                                            "args": {"command": "echo 'step 1'"},
                                            "id": "call_turn_1"
                                        }
                                    }
                                ]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 20, "candidatesTokenCount": 10, "totalTokenCount": 30}
                }
            elif len(fn_responses) == 1:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [
                                    {
                                        "functionCall": {
                                            "name": "bash",
                                            "args": {"command": "echo 'step 2'"},
                                            "id": "call_turn_2"
                                        }
                                    }
                                ]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 40, "candidatesTokenCount": 10, "totalTokenCount": 50}
                }
            else:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [{"text": "Multi-turn tool completed successfully!"}]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 60, "candidatesTokenCount": 10, "totalTokenCount": 70}
                }
            if "alt=sse" in path or ":streamGenerateContent" in path:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/check-project-context" in path:
            sys_inst = req_json.get("systemInstruction", "")
            if isinstance(sys_inst, dict):
                parts = sys_inst.get("parts", [])
                sys_inst_text = " ".join(p.get("text", "") for p in parts if isinstance(p, dict))
            else:
                sys_inst_text = str(sys_inst)
            if "project_context" in sys_inst_text and "CLAUDE.md" in sys_inst_text:
                reply = "Project context verified: CLAUDE.md is present"
            else:
                reply = "Project context missing"
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": [{"text": reply}]},
                        "finishReason": "STOP"
                    }
                ],
                "usageMetadata": {"promptTokenCount": 20, "candidatesTokenCount": 10, "totalTokenCount": 30}
            }
            if "alt=sse" in path or ":streamGenerateContent" in path:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()
                self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "alt=sse" in path or ":streamGenerateContent" in path:
            # 模拟 SSE 流式响应
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.end_headers()
            
            if "/stream-idle-hang" in path:
                # 模拟输出部分 chunk 后静默挂起超时
                chunk1 = {"candidates": [{"content": {"role": "model", "parts": [{"text": "Start..."}]}}]}
                self.wfile.write(f"data: {json.dumps(chunk1)}\n\n".encode("utf-8"))
                self.wfile.flush()
                import time
                time.sleep(30)
                return

            if "/stream-function-call" in path:
                chunk1 = {
                    "candidates": [{
                        "content": {
                            "role": "model",
                            "parts": [{
                                "functionCall": {
                                    "name": "bash",
                                    "args": {"command": "ls -la"},
                                    "id": "stream_call_1"
                                }
                            }]
                        },
                        "finishReason": "STOP"
                    }],
                    "usageMetadata": {"promptTokenCount": 10, "candidatesTokenCount": 5, "totalTokenCount": 15}
                }
                self.wfile.write(f"data: {json.dumps(chunk1)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return

            # 默认 SSE 流式：分2个 chunk 发送
            chunk1 = {
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "Hello "}]
                    }
                }]
            }
            chunk2 = {
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "stream!"}]
                    },
                    "finishReason": "STOP"
                }],
                "usageMetadata": {
                    "promptTokenCount": 10,
                    "candidatesTokenCount": 6,
                    "totalTokenCount": 16
                }
            }
            self.wfile.write(f"data: {json.dumps(chunk1)}\n\n".encode("utf-8"))
            self.wfile.flush()
            self.wfile.write(f"data: {json.dumps(chunk2)}\n\n".encode("utf-8"))
            self.wfile.flush()
            return

        # default echo ok
        resp = {
            "candidates": [
                {
                    "content": {
                        "role": "model",
                        "parts": [{"text": "Hello from stub!"}]
                    },
                    "finishReason": "STOP"
                }
            ],
            "usageMetadata": {
                "promptTokenCount": 15,
                "candidatesTokenCount": 5,
                "totalTokenCount": 20
            },
            "echo": {
                "apiKey": self.headers.get("x-goog-api-key"),
                "body": req_json
            }
        }
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps(resp).encode("utf-8"))

    def log_message(self, format, *args):
        pass

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    daemon_threads = True

def main():
    httpd = ThreadedTCPServer(("127.0.0.1", 0), StubHandler)
    port = httpd.server_address[1]
    # Print port to stdout and flush so child process waiter can read it
    sys.stdout.write(f"PORT:{port}\n")
    sys.stdout.flush()
    httpd.serve_forever()

if __name__ == "__main__":
    main()
