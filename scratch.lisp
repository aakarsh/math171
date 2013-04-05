(bfs "s" "t" *g*)

(defparameter *nodes* (graph-nodes *g*))

(bfs-pred-path->string (list  (list (car *nodes*) (second *nodes*))))
(str/join-words (list "hello" "world"))


;; (defun enqueue (n q)
;;   (setf q (push n q)))

(defmacro enqueue (n q)
  `(setf ,q (push ,n ,q)))


(bfs-enqueue  (first (graph-nodes *g*)) q)



(defparameter q '())

(enqueue 'a q)
(enqueue 'b q)

