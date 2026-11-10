;; pi-model-metadata?
;; 判断对象是否为模型规格元数据 record 实例。
;;
;; 语法
;; ----
;; (pi-model-metadata? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于模型规格元数据 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-model-metadata "gemini-2.5-flash" "Gemini 2.5 Flash"
             "google" "https://generativelanguage.googleapis.com" 1048576 65536
             #t
           ) ;%make-pi-model-metadata
      ) ;obj
     ) ;
  (check-true (pi-model-metadata? obj))
) ;let
(check-false (pi-model-metadata? '()))
(check-false (pi-model-metadata? #f))
(check-false (pi-model-metadata? "not-a-record"))

(check-report)
