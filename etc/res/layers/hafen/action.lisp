(in-package :hlu)

(deflayer action (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      :string "pr"
      :uint16 "pver"
      :string "name"
      :string "preq (ignored)"
      :uint16 "hk"
      (let ((ad :uint16 "ad (Don't change ads...)"))
        (dotimes (i ad)
        :string ((format nil "ad(~A) [Don't change ads...]" i))))))
  
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      :string
      :uint16
      :string
      :string
      :uint16
      (let ((ad :uint16))
        (dotimes (i ad)
          :string)))))

