(in-package :hlu)

(deflayer tileset2 (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      (do (())
          ((msg-eom msg))
        (let ((p :uint8 "p"))
          (case p
            (0 :string "tn"
               :dlist)
            (1
             (let ((flnum :uint16 "flnum"))
               :uint16 "flavprob"
               (dotimes (i flnum)
                 :string ((format nil "fln[~A]" i))
                 :uint16 ((format nil "flv[~A]" i))
                 :uint8  ((format nil "flw[~A]" i)))))
            (2
             (let ((len :int8 "tag-length"))
               (dotimes (i len)
                 :string ((format nil "Tag[~A]" i)))))
            (t (error "Invalid p in tileset2")))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      (format t "Encode~%")
      (do ((p (next-input in)
              (next-input in)))
          ((null p))
        (setf p (parse-integer p))
        (eint out p 1)
        (case p
          (0 :string
             :dlist)
          (1 (let ((flnum :uint16))
               (ntimes flnum
                 :string
                 :uint16
                 :uint8)))
          (2 (let ((len :int8))
               (ntimes len
                 :string)))
          (t (error "Invalid p in tileset2")))))))
