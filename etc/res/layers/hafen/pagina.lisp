(in-package :hlu)

(deflayer pagina (nil)
  (lambda (msg fn)
    (decoder msg nil
      :raw :all (strcat fn ".ini")))
  (lambda (fn out)
    (encoder nil out
      :raw (strcat fn ".ini"))))
