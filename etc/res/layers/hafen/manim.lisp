(in-package :hlu)

(deflayer manim (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      (let ((ver :uint8 "verison"))
        (if (= ver 1)
            (progn
              :int16 "id"
              :uint8 "random (1 or 0)"
              :float32 "length"
              (block loop
                (loop
                   until (msg-eom msg)
                   do (progn
                        (let ((ty :uint8 "type"))
                          :float32 "tm"
                          (let ((n :uint16 "n"))
                            (dotimes (i n)
                              :uint16 "st"
                              (let ((run :uint16 "run"))
                                (dotimes (o run)
                                  (cond
                                    ((= ty 0)
                                     (return-from loop))
                                    ((= ty 1)
                                     :float32 ((format nil "pos[~A]~%" (* i 3))
                                               (format nil "pos[~A]~%" (+ (* i 3) 1))
                                               (format nil "pos[~A]~%" (+ (* i 3) 2))
                                               (format nil "nrm[~A]~%" (* i 3))
                                               (format nil "nrm[~A]~%" (+ (* i 3) 1))
                                               (format nil "nrm[~A]~%" (+ (* i 3) 2))))
                                               
                                    ((= ty 2)
                                     :int32 "some float9995, ignore")
                                    ((= ty 3)
                                     (ntimes 3
                                       :float32 "hfdec"))
                                    (t
                                     (error "unknown type for manim"))))))))))))
            (error "invalid version for manim")))))
  (lambda (fn out in)
    (declare (ignore fn out in))
    (error "manim hasn't been defined for encoding yet")))
