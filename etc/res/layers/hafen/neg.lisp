(in-package :hlu)

(deflayer neg (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      :coord ("cc" "bc (ignored)" "bs (ignored)" "sz (ignored)")
      (let ((en :uint8 "en"))
        (dotimes (i en)
          (let ((epid :uint8 "epid"))
            (let ((cn :uint16 "cn"))
              (dotimes (j cn)
                :coord ((format nil "ep[~A][~A]" epid j)))))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      (ntimes 4
        :coord)
      (let ((en :uint8))
        (dotimes (i en)
          :uint8
          (let ((cn :uint16))
            (dotimes (j cn)
              :coord)))))))
