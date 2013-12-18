;; -*- mode: common-lisp; coding: utf-8 -*-
(in-package :cl-smtp)

(defparameter *cl-smtp-tests* (make-hash-table :test 'equal))

(defmacro define-cl-smtp-test (name (&rest args) &body body)
  (let ((tmpname (gensym (string name))))
    `(progn
       (defun ,tmpname (,@(mapcar #'car args))
         ,@body)
       (setf (gethash ,(string-downcase name) *cl-smtp-tests*)
             (list #',tmpname ,args)))))

(defun get-component-pathname ()
  (asdf:component-pathname (asdf:find-system "cl-smtp")))



(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-1" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :utf-8))))
    (assert qstr)
    (assert (string-equal qstr "=C3=B6=C3=BC=C3=A4=C3=96=C3=9C=C3=84=C3=9F"))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-2" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1))))
    (assert qstr)
    (assert (string-equal qstr "=F6=FC=E4=D6=DC=C4=DF"))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-3" ()
  (let* ((str "check if #\= encoded")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1))))
    (assert qstr)
    (assert (string-equal qstr "check if #\=3D encoded"))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-4" ()
  (let* ((str "Müde vom Durchwandern öder Letternwüsten, voll leerer Hirngeburten, in anmaaßendsten Wortnebeln ; überdrüssig ästhetischer Süßler wie grammatischer Wässerer ; entschloß ich mich : Alles, was je schrieb, in Liebe und Haß, als immerfort mitlebend zu behandeln !")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1 :columns 64))))
    (assert qstr)
    (assert (string-equal qstr "M=FCde vom Durchwandern =F6der Letternw=FCsten, voll leerer Hirngeburt=
en, in anmaa=DFendsten Wortnebeln ; =FCberdr=FCssig =E4sthetischer S=FC=DFle=
r wie grammatischer W=E4sserer ; entschlo=DF ich mich : Alles, was j=
e schrieb, in Liebe und Ha=DF, als immerfort mitlebend zu behandel=
n !"
))))

(define-cl-smtp-test "string-has-non-ascii-1" ()
  (assert (string-has-non-ascii "test Ü ende")))

(define-cl-smtp-test "string-has-non-ascii-2" ()
  (assert (not (string-has-non-ascii "test ende"))))

(define-cl-smtp-test "rfc2045-q-encode-string-utf-8" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (rfc2045-q-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal 
             qstr "=?UTF-8?Q?=C3=B6=C3=BC=C3=A4=C3=96=C3=9C=C3=84=C3=9F?="))))

(define-cl-smtp-test "escape-rfc822-quoted-string" ()
  (assert (equal (escape-rfc822-quoted-string "test end") "test end"))
  (assert (equal (escape-rfc822-quoted-string "test\\end") 
                 "test\\\\end"))
  (assert (equal (escape-rfc822-quoted-string "test\"end")
                 "test\\\"end"))
  (assert (equal (escape-rfc822-quoted-string (format nil "test~%end")) 
                 (format nil "test\\~%end")))
  (assert (equal (escape-rfc822-quoted-string 
                  (format nil "test~cend" #\Return)) 
                 (format nil "test\\~cend" #\Return)))
  (assert (equal (escape-rfc822-quoted-string "test/end") "test/end"))
  (assert (equal (escape-rfc822-quoted-string "test end\\") 
                 "test end\\\\"))
  (assert (equal (escape-rfc822-quoted-string (format nil "~%test end\\")) 
                 (format nil "\\~%test end\\\\"))))

(define-cl-smtp-test "rfc2231-encode-string-utf-8" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (rfc2231-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal 
             qstr "UTF-8''%C3%B6%C3%BC%C3%A4%C3%96%C3%9C%C3%84%C3%9F"))
    ))

(define-cl-smtp-test "make-attachment-1" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p)))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "make-attachment-2" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment p))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "make-attachment-3" ()
  (let* ((p (namestring (merge-pathnames "tests.lisp" 
                                         (get-component-pathname))))
         (attachment p))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "send-attachment-header-1" ()
  (let* ((boundary (make-random-boundary))
         (p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p))
         (headerstr (with-output-to-string (s)
                      (send-attachment-header s boundary attachment :utf-8)))
         (returnnewline (format nil (format nil "~C~C" #\Return #\NewLine)))
         (tmpstr (format nil "--~A~AContent-type: text/plain;~% name*=UTF-8''tests.lisp;~% name=\"tests.lisp\"~AContent-Disposition: attachment; filename*=UTF-8''tests.lisp; filename=\"tests.lisp\"~AContent-Transfer-Encoding: base64~A~A" 
                         boundary returnnewline returnnewline returnnewline 
                         returnnewline returnnewline)))
    (assert (equal headerstr tmpstr))
    ))

(define-cl-smtp-test "send-attachment-header-2" ()
  (let* ((boundary (make-random-boundary))
         (p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p
				      :mime-type "text/plain"
				      :name "foo\\bar"))
         (headerstr (with-output-to-string (s)
                      (send-attachment-header s boundary attachment :utf-8)))
         (returnnewline (format nil (format nil "~C~C" #\Return #\NewLine)))
         (tmpstr (format nil "--~A~AContent-type: text/plain;~% name*=UTF-8''foo%5cbar;~% name=\"foo\\\\bar\"~AContent-Disposition: attachment; filename*=UTF-8''foo%5cbar; filename=\"foo\\\\bar\"~AContent-Transfer-Encoding: base64~A~A" 
                         boundary returnnewline returnnewline returnnewline 
                         returnnewline returnnewline)))
        (print headerstr)
    (print tmpstr)

    (assert (equal headerstr tmpstr))
    ))


(define-cl-smtp-test "mask-dot-1" ()
  (assert (equal (mask-dot (format nil "~C~C.~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C..~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C~C..~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C..~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C~C~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C.~C.~C.~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C.~C.~C.~C" #\Return #\NewLine
                         #\Return #\NewLine))))

(define-cl-smtp-test "substitute-return-newline" ()
  (assert (equal (substitute-return-newline 
                  (format nil "start~Aende" *return-newline*))
                 "start ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~Aweiter~Aende" 
                          *return-newline* *return-newline*))
                 "start weiter ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "~Astart~Aweiter~Aende~A" 
                          *return-newline* *return-newline* *return-newline*
                          *return-newline*))
                 " start weiter ende "))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~Aende" 
                          *return-newline* *return-newline*))
                 "start  ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~A~Aende" 
                          *return-newline* *return-newline* *return-newline*))
                 "start   ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~%~A~Aende" 
                          *return-newline* *return-newline* *return-newline*))
                 "start 
  ende"))
  )

(defun file-to-usb8-buffer (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((flength (file-length s))
           (buffer (make-array flength :element-type '(unsigned-byte 8))))
      (loop for i from 0 to flength do
           (let ((bchar (read-byte s nil 'EOF)))
             (if (eql bchar 'EOF)
                 (return)
                 (setf (aref buffer i) bchar))))
      buffer)))

(define-cl-smtp-test "base64-encode-file" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (base64str1 (with-output-to-string (s)
                       (base64-encode-file p s)))
         (buffer (file-to-usb8-buffer p))
         (base64str2 
          #-allegro
           (cl-base64:usb8-array-to-base64-string buffer :columns 0)
          #+allegro 
          (excl:usb8-array-to-base64-string buffer :wrap-at-column nil)
           )) 
    
    (assert (string-equal (remove #\Return (remove #\Newline base64str1 :test #'equal) :test #'equal) base64str2))
    ))

(defun run-test (name)
  (handler-case
      (let ((test (gethash name *cl-smtp-tests*)))
        (format t "~%run test: ~S ~@[(~A)~]~%" name (cadr test))
        (apply (car test) (cadr test))
        (format t "pass~%")
        t)
    (simple-error (c)
      (format t "failed: ~A" c)
      nil)))

(defun run-tests ()
  (let ((n (hash-table-count *cl-smtp-tests*))
        (pass 0))
    (format t "~%run ~D cl-smtp-tests~%~%" (hash-table-count *cl-smtp-tests*))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (when (run-test k)
                   (incf pass)))
             *cl-smtp-tests*)
    (format t "~%pass: ~D | failed: ~D~%~%" pass (- n pass))))
