(in-package :hlu)

(deflayer windowconfig (t)
  (lambda (msg fn out)
    (decoder msg out
      :coord "top left corner of usable space"
      :coord "bottom right corner of usable space"
      :coord "caption position"
      :coord "button top right position"))
  (lambda (fn out in)
    (encoder in out
      (ntimes 4
        :coord))))