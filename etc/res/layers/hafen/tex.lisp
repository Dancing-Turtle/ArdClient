(in-package :hlu)

(deflayer tex (t)
  (lambda (msg fn out)
    (decoder msg out
      :int16 "id"
      :coord ("off" "sz")
      (loop
         until (msg-eom msg)
         do (let ((type :uint8 "t"))
              (case type
                (0 (let ((len :uint32 "img-size"))
                     :raw len (strcat fn ".png")))
                (1 :uint8 "ma")
                (2 :uint8 "magf")
                (3 :uint8 "minf")
                (4 (let ((len :uint32 "mask-size"))
                     :raw len (strcat fn ".mask.png")))
                (t (error "Invalid type for tex")))))))
  (lambda (fn out in)
    (encoder in out
      :int16
      (ntimes 2 :coord)
      (do ((p (next-input in)
              (next-input in)))
          ((null p))
        (setf p (parse-integer p))
        (eint out p 1)
        (case p
          (0 :uint32
             :raw (strcat fn ".png"))
          (1 :uint8)
          (2 :uint8)
          (3 :uint8)
          (4 :uint32
             :raw (strcat fn ".mask.png"))
          (t (error "Invalid type of tex")))))))
