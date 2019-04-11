(in-package :hlu)

(deflayer light (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      :int16 "id"
      :fcolor ("amb" "dif" "spc")
      (loop
         until (msg-eom msg)
         do (let ((type :uint8 "type"))
              (case type
                (1 :cpfloat "ac"
                   :cpfloat "al"
                   :cpfloat "aq")
                (2 :cpfloat "x"
                   :cpfloat "y"
                   :cpfloat "z")
                (3 :cpfloat "exp")
                (t (error "Invalid type in light")))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      :int16
      (ntimes 4 :fcolor)
      (do ((type (next-input in)
                 (next-input in)))
          ((null type))
        (setf type (parse-integer type))
        (eint out type 1)
        (case type
          (1 :cpfloat
             :cpfloat
             :cpfloat)
          (2 :cpfloat
             :cpfloat
             :cpfloat)
          (3 :cpfloat)
          (t (error "Invalid type in light"))))))) 
             
