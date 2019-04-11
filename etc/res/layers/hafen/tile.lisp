(in-package :hlu)

(deflayer tile (t)
  (lambda (msg fn out)
    (decoder msg out
      :uint8 ("t" "id")
      :uint16 "w"
      :raw :all (strcat fn ".png")))
  (lambda (fn out in)
    (encoder in out
      (ntimes 2
        :uint8)
      :uint16
      :raw (strcat fn ".png"))))
