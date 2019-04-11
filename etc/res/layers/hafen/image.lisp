(in-package :hlu)

(deflayer image (t)
  (lambda (msg fn out)
    (decoder msg out
      :int16 "z"
      :int16 "subz"
      :uint8 "fl"
      :int16 "id"
      :coord "O"
      :raw :all (strcat fn ".png")))
  (lambda (fn out in)
    (encoder in out
      (ntimes 2
        :int16)
      :uint8
      :int16
      :coord
      :raw (strcat fn ".png"))))
