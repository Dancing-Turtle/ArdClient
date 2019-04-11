(in-package :hlu)

(deflayer foodev (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      (let ((ver :uint8 "version"))
        (if (= ver 1)
            (progn
              :color "col"
              :string "nm"
              :int16 "sort")
            (error "Invalid version of foodev")))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      (let ((ver :uint8))
        (if (= ver 1)
            (progn
              :color
              :string
              :int16)
            (error "Invalid version of foodev"))))))
