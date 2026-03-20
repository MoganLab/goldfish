;
; Copyright (C) 2025 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check) (liii hashlib) (liii path))

(check (md5 "") => "d41d8cd98f00b204e9800998ecf8427e")
(check (md5 "hello") => "5d41402abc4b2a76b9719d911017c592")
(check (md5 "The quick brown fox jumps over the lazy dog") => "9e107d9d372bb6826bd81d3542a419d6")
(check (md5 "a") => "0cc175b9c0f1b6a831c399e269772661")
(check (md5 "123456") => "e10adc3949ba59abbe56e057f20f883e")
(check (md5 "!@#$%^&*()") => "05b28d17a7b6e7024b6e5d8cc43a8bf7")
(check (md5 "Hello") => "8b1a9953c4611296a827abf8c47804d7")

;; SHA1 tests
(check (sha1 "") => "da39a3ee5e6b4b0d3255bfef95601890afd80709")
(check (sha1 "hello") => "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d")
(check (sha1 "The quick brown fox jumps over the lazy dog") => "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")
(check (sha1 "a") => "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")
(check (sha1 "123456") => "7c4a8d09ca3762af61e59520943dc26494f8941b")
(check (sha1 "!@#$%^&*()") => "bf24d65c9bb05b9b814a966940bcfa50767c8a8d")
(check (sha1 "Hello") => "f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0")

;; SHA256 tests
(check (sha256 "") => "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
(check (sha256 "hello") => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
(check (sha256 "The quick brown fox jumps over the lazy dog") => "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")
(check (sha256 "a") => "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
(check (sha256 "123456") => "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92")
(check (sha256 "!@#$%^&*()") => "95ce789c5c9d18490972709838ca3a9719094bca3ac16332cfec0652b0236141")
(check (sha256 "Hello") => "185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969")

(let ((tmp-file "tests/resources/hashlib-test-temp.txt")
      (content "hello"))
  (path-write-text tmp-file content)
  (check (md5-by-file tmp-file) => (md5 content))
  (check (sha1-by-file tmp-file) => (sha1 content))
  (check (sha256-by-file tmp-file) => (sha256 content))

  (path-write-text tmp-file "")
  (check (md5-by-file tmp-file) => (md5 ""))
  (check (sha1-by-file tmp-file) => (sha1 ""))
  (check (sha256-by-file tmp-file) => (sha256 ""))
  (delete-file tmp-file)
) ;let

;; Large file hash test (local deterministic data, no network dependency)
(let* ((large-file "tests/resources/hashlib-test-large-local.txt")
       (large-size (* 100 1024 1024)) ; 100MB
       (large-content (make-string large-size #\A)))
  (path-write-text large-file large-content)
  (check (path-getsize large-file) => large-size)
  (check (md5-by-file large-file) => (md5 large-content))
  (check (sha1-by-file large-file) => (sha1 large-content))
  (check (sha256-by-file large-file) => (sha256 large-content))
  (when (path-exists? large-file)
    (delete-file large-file)
  ) ;when
) ;let*



(check-report)
