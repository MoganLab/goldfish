#!/usr/bin/env python3
"""Vertex OpenAI 兼容端点：必须携带 x-goog-api-key，否则返回 401。"""

import json
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def reply(self, status, payload):
        body = json.dumps(payload).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):  # noqa: N802
        length = int(self.headers.get("content-length", 0) or 0)
        request = json.loads(self.rfile.read(length) or b"{}")
        api_key = self.headers.get("x-goog-api-key")
        if api_key != "test-key":
            self.reply(401, {"error": {"code": 401, "message": "API key not valid."}})
            return

        content = "header-ok:" + api_key
        if request.get("stream"):
            chunks = [
                {"id": "vertex-stub", "object": "chat.completion.chunk", "choices": [{"index": 0, "delta": {"role": "assistant", "content": content}, "finish_reason": None}]},
                {"id": "vertex-stub", "object": "chat.completion.chunk", "choices": [{"index": 0, "delta": {}, "finish_reason": "stop"}], "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}},
            ]
            body = "".join("data: " + json.dumps(chunk) + "\n\n" for chunk in chunks) + "data: [DONE]\n\n"
            payload = body.encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
            return
        self.reply(200, {
            "id": "vertex-stub",
            "object": "chat.completion",
            "model": "vertex-stub",
            "choices": [{"index": 0, "message": {"role": "assistant", "content": content}, "finish_reason": "stop"}],
            "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}
        })

    def log_message(self, *args):
        pass


if __name__ == "__main__":
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    print("PORT:%d" % server.server_address[1], flush=True)
    server.serve_forever()
