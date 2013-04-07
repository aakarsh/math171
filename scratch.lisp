(defun find-full-name (name)
  (loop 
     for full-name in  (professor-map->names *professor-map*)
     for last_name = (last-name (string->name full-name))         
       do 
       (if (string= last_name name)
           (return full-name))))
(defun find-facuty-full-names (last-names)
  (loop 
     for name in last-names
     for full-name =(find-full-name name)
     with unkown = '()
     finally  (progn
                (format t "~%")
                (loop for u in unkown do
                     (format t " \"~a\" " u))
                ( return unkown))
     do          
       (if full-name
           (format t "\"~a\" "  full-name)
           (push name unkown))))

(find-facuty-full-names *ft-faculty*)


(defparameter "s ahmed" "s arabhi" "m bodas" "s desousa" "c fan" "t fish" "t huynh" "k jensen" "a jiru" "j jordan" "o kovaleva" "r low" "j lum" "w newball" "l papay" "l sega" "t smith" "a strong" "a talebi" "p tanniru" "j trubey" "j wang" "e zabric" "m zoubeidi" "verga"  "varitanian"  "van der poel"  "a tran"  "rober"  "rigers"  "v nguyen"  "t nguyen"  "a nguyen"  "hillard") 





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

