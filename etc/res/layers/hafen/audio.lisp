(in-package :hlu)

(deflayer audio (nil)
  (lambda (msg fn)
    (decoder msg nil
      :raw :all (strcat fn ".ogg")))
  (lambda (fn out)
    (encoder nil out
      :raw (strcat fn ".ogg"))))


      
