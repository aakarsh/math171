(in-package :com.bovinasancta.util)


(defun hash-keys (hash-table)
  (loop for key being
        the hash-keys of hash-table
        collect key))

(defun take(n ls)
  (loop for i from 0 to n
        for l  = ls then (cdr l)
        while (< i n)
        collect (car l)))

(defun split-by-char (str &optional (c #\Space))
  (when (and str (> (length str) 0))
    (loop for i = 0 then (1+ j)
          as j = (position c str :start i)
          collect (subseq str i j)
          while j)))

(defun str/split-by-one-space (str)
    "Returns a list of substrs of str divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
str between them."
    (split-by-char str #\Space))


(defun str/join-words(words &optional (sep " "))
  (loop for word in words
        for result = word then (concatenate 'string result sep word)
       finally (return result)))

(defun ass/val (key ls)
  (let ((pair (assoc key ls)))
    (if pair
        (cadr pair) nil)))


(defun string->list (str)
  (concatenate 'list str))

(defun list-intersect(ls1 ls2)
  (loop for l in ls1
        do
        (if (member l ls2)
            (return t))))

(defun betweenp (a i1 i2)
  (and (>= a i1)  (<= a i2)))
