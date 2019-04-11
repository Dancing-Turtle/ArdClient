(in-package :hlu)

(deflayer clamb (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      (let ((ver :uint8 "Version"))
        (when (or (< ver 1)
                  (> ver 2))
          (error "Invalid version in clamb"))
        (when (>= ver 2)
          (let ((nm :string "pnm"))
            (when (plusp (length nm))
              :uint16 "pnm version")))
        :uint8 ("minc" "maxc")
        :uint16 "maxi"
        :float32 "bvol"
        (let ((cnms :uint8 "cnms-length"))
          (dotimes (i cnms)
            :string ((format nil "cnms[~A]" i))
            :float32 ((format nil "ieps[~A] = 1/val" i)))))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      (let ((ver :uint8))
        (when (or (< ver 1)
                  (> ver 2))
          (error "Invalid version in clamb"))
        (when (>= ver 2)
          (let ((nm :string))
            (when (plusp (length nm))
              :uint16)))
        :uint8
        :uint8
        :uint16
        :float32
        (let ((cnms :uint8))
          (dotimes (i cnms)
            :string
            :float32))))))
      
