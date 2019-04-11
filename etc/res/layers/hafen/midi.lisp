(in-package :hlu)

(deflayer midi (nil)
  (lambda (msg fn)
    (decoder msg nil
      :raw :all (strcat fn ".midi")))
  (lambda (fn out)
    (encoder nil out
      :raw (strcat fn ".midi"))))
