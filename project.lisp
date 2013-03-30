

(defvar *pt-faculty*
  (list "Ahmed" "Arabhi" "Bodas" "DeSousa" "Fan" "Fish" "Hillard" "Huynh"
        "Jensen" "Jiru" "Jordan" "Kovaleva" "Low" "Lum" "Newball" "A. Nguyen"
        "T Nguyen" "V Nguyen" "Papay" "Rigers" "Rober" "Sega" "Smith" "Strong"
        "Talebi" "Tanniru" "A. Tran" "Trubey" "Van der Poel" "Varitanian" "Verga"
        "Wang" "Zabric" "Zoubeidi")
  "List of part time faculty")


(defvar *ft-faculty*
  (list "Alperin" "Becker" "Beason" "Blockus" "Dodd" "Foster"
        "Hsu" "Jackson" "Katsura" "Kellum" "Kubelka" "Ng" "Obaid" "Pence"
        "Peterson" "Pfifer" "Rivera" "Roddic" "Saleem" "Schmeichel" "Shubin"
        "Sliva-Spitzer" "So" "Stanley")
  "List of full time faculty")

(defvar *ass-faculty*
  (list "Bremer" "Cayco" "Crunk" "Gottlieb" "Koev" "Lee" "Maruskin" "Simic")
        "List of Associate or Assistant Professors")

(defvar *sample-graph-file* "/home/aakarsh/src/prv/common-lisp/clisp-sample/sample-graph.lisp")
;; (defconstant *graph1* '((:vertex :name "v1" :source t  :neighbours '((:vertex :name v2 :edge-capacity 3) 
;;                                                                      (:vertex :name v2 :edge-capacity 4)

;; ))))

(defvar *tg* "/home/aakarsh/src/prv/common-lisp/clisp-sample/simple.txt")

(defvar g (an/parse-graph *tg*))


(defclass edge()
  ((start :initarg :start-node)
   (end :initarg :end-node)
   (capacity :initarg :capacity :initform 0)))

(defclass node()
  ((name :initarg :name)
   (color :initarg :color :initform "W")
   (neighbours :initarg :adj :initform '())))

(defclass graph()
  ((nodes :initarg :nodes)
   (edges :initarg :edges)))

(defun graph-edges(g)
  (slot-value g 'edges))

(defvar es (graph-edges g))

(defun graph-nodes(g)
  (slot-value g 'nodes))

(defun node-count(g)
  (length (graph-nodes g))

(defun edge-count(g)
  (length (graph-edges g)))

(defun edges->node-neighbours(n edges)
  (let ((adjv '()))
    (loop 
       for e in edges do
         (if (string= n (slot-value e 'start))
               (push (slot-value e 'end) adjv)))
    (remove-duplicates adjv :test #'string=)))

(defun find-node(name nodes)
  (loop 
     for node in nodes 
     for k = nil do 
     (if (string= name (slot-value node 'name))
         (progn
           (return node)))))

(defun edges->nodes(edges)
  (let ((nodes '())
        (cur_node '()))    
    (loop 
       for e in edges 
       with end_vertex_name    = "e" ;;(slot-value e 'end)
       with start_vertex_name = "s" ;;(slot-value e 'start)
       for cur_node = '() then 
           (find-node start_vertex_name nodes)
      do
         (if (not cur_node)
             (push (make-instance 'node :name start_vertex_name)  nodes)
             (progn
               (format t "~a ~%" (slot-value cur_node :name))
               (push cur_node nodes))))
  nodes))

(edges->nodes (graph-edges g))

;;(defun bfs(g n)
;;  (let ((path '()))
           
   


 (defun an/split-by-one-space (str)
    "Returns a list of substrs of str divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
str between them."
    (when (and str (> (length str) 0))
    (loop for i = 0 then (1+ j)
          as j = (position #\Space str :start i)
          collect (subseq str i j)
          while j)))



(defun edges->node-names(edges)
  (let ((node-names '()))
    (dolist (e edges)
      (push (slot-value e 'start) node-names)
      (push (slot-value e 'end) node-names))
    (setq node-names (remove-duplicates node-names :test #'string= ))))


;; (defun an/uniq(ls &optional (cmp #'eql))
;;   (let (retval '())
;;     (loop for l in ls
;;        do 
;;          (if (not (position l retval :test cmp))
;;              (push l retval))
;;        finally (return (nreverse retval)))))


(defun parse-graph(fn)
  (let* ((edges (an/read-edge-file fn))
         (nodes (edges->node-names edges)))
    (make-instance 'graph 
                   :nodes nodes
                   :edges edges)))

(defun an/read-edge-file(fn)
  (let ((in (open fn :if-does-not-exist nil))
        (edges '()))
    (when in
      (loop for line = (read-line in nil)
         while line do 
           (let ((l (an/split-by-one-space line)))
             (when (eql (length l) 3)
               (push 
                (make-instance 'edge 
                               :start-node (nth 0 l) 
                               :end-node (nth 1 l)
                               :capacity (parse-integer (nth 2 l)))
                edges)))))
    (close in)
    (nreverse edges)))
    
  


(defun an/read-sample-graph()
  (let ((in (open *sample-graph-file* :if-does-not-exist nil)))
    (when in
      (loop for line = (read in nil)
            while line do
            (format t "~a~%" line)))
    (close in)))
