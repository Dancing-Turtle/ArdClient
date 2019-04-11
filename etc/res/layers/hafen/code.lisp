(in-package :hlu)

(deflayer code (t)
  (lambda (msg fn out)
    (decoder msg out
      (let ((name :string "name (You shouldn't edit this)"))
        :raw :all (strcat fn name ".class"))))
  (lambda (fn out in)
    (encoder in out
      (let ((name :string))
        :raw (strcat fn name ".class")))))
