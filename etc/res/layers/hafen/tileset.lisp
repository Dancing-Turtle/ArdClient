(in-package :hlu)

(deflayer tileset (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (format out ";;Legacy tileset~%")
    (decoder msg out
      :uint8 "fl"
      (let ((flnum :uint16 "flnum"))
        :uint16 "flavprob"
        (dotimes (i flnum)
          :string ((format nil "fln[~A]" i))
          :uint16 ((format nil "flv[~A]" i))
          :uint8  ((format nil "flw[~A]" i))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      :uint8
      (let ((flnum :uint16))
        :uint16
        (ntimes flnum
          :string
          :uint16
          :uint8)))))
