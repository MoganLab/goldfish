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

        if "/429-then-ok" in path:
            server = self.server
            server.request_count = getattr(server, "request_count", 0) + 1
            if server.request_count < 3:
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

def main():
    httpd = socketserver.TCPServer(("127.0.0.1", 0), StubHandler)
    port = httpd.server_address[1]
    # Print port to stdout and flush so child process waiter can read it
    sys.stdout.write(f"PORT:{port}\n")
    sys.stdout.flush()
    httpd.serve_forever()

if __name__ == "__main__":
    main()
