(in-package :cl-user)

(defpackage :gk-user
  (:use :cl :cl-user)
  (:export 
   :qload
   :clu
   :def
   :cat
   :wostr
   :wof
   :file->string
   :file->base64
   :string->base64
   :vec
   :vec-push
   :vec-add
   :vec-get
   :vec-length
   :vec-last-num
   :fpath)
  (:nicknames :gk))
   


;;;;;; General

(defmacro qload (&rest body) `(ql:quickload ,@body))
(defmacro clu () `(in-package :cl-user))
(defmacro def (&rest body) `(defparameter ,@body))
(defmacro cat (&rest body) `(concatenate 'string ,@body))

;;;;; IO

(defmacro wostr (&rest body) `(with-output-to-string ,@body))
(defmacro wof (&rest body) `(with-open-file ,@body))
(defun file->string (fn)
  (when (pathnamep fn)
    (with-output-to-string (s)
      (with-open-file (ifh fn)
	   (when ifh 
	     (loop for l = (read-line ifh nil)
		  while l do (format s "~a~%" l)))))))

;;;; Base64

(qload "s-base64")

(defun file->base64 (fn)
  "IN: file name
   OUT: base64 string of its contents"
  (with-output-to-string 
      (s)
    (with-open-file (f *fn* :direction :input :element-type '(unsigned-byte 8))
      (loop for c = (read-byte f nil nil) while c do
	   (s-base64:encode-base64 f s)))))

(defun string->base64 (a-str)
  (wostr (s) (s-base64:encode-base64-bytes (map 'vector #'char-code a-str) s)))

;;;; vectors
;;;;

(defmacro vec () `(make-array 1 :fill-pointer 0 :adjustable t))
(defmacro vec-push (a-vec a-elt) `(vector-push-extend ,a-elt ,a-vec))
(defmacro vec-add (&rest body) `(vec-push ,@body))
(defmacro vec-get (a-vec a-num) `(elt ,a-vec ,a-num))
(defmacro vec-length (a-vec) `(length ,a-vec))
(defmacro vec-last-num (a-vec) `(1- (length ,a-vec)))

;;;; file paths

(defun fpath (a-dir a-fname)
  "dir-path + string -> file-path"
  (make-pathname :directory (directory-namestring a-dir) :name a-fname))

;;;