(in-package :hlu)

(deflayer codeentry (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      (loop
         until (msg-eom msg)
        do (let ((ty :uint8 "t"))
             (case ty
               (1 :string "en"
                  :string "cn")
               (2 (let ((ln :string "ln"))
                    (when (plusp (length ln))
                      :uint16 "version")))
               (t (error "Invalid t for codeentry")))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      (do ((p (next-input in)
              (next-input in)))
          ((null p))
        (setf p (parse-integer p))
        (eint out p 1)
        (case p
          (1 :string :string)
          (2 (let ((ln :string))
               (when (plusp (length ln))
                 :uint16)))
          (t (error "Invalid t for codeentry")))))))
