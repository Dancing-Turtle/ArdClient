(in-package :hlu)

(deflayer anim (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      :int16 "id"
      :uint16 "d"
      (let ((ids :uint16 "ids-length"))
        (dotimes (i ids)
          :int16 ((format nil "ids[~A]" i))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      :int16
      :uint16
      (let ((ids :uint16))
        (ntimes ids
          :int16)))))
