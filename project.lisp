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

(defvar *sample-graph-file*
  "/home/aakarsh/src/prv/common-lisp/clisp-sample/sample-graph.lisp")

(defvar *tg* "/home/aakarsh/src/prv/common-lisp/graphs/simple.txt")

(defun take(n ls)
  (loop for i from 0 to n
        for l  = ls then (cdr l)
        while (< i n)
        collect (car l)))

(defun split-by-one-space (str)
    "Returns a list of substrs of str divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
str between them."
    (when (and str (> (length str) 0))
    (loop for i = 0 then (1+ j)
          as j = (position #\Space str :start i)
          collect (subseq str i j)
          while j)))


(defclass edge()
  ((start :initarg :start-node)
   (end :initarg :end-node)
   (flow :initarg :flow :initform 0)
   (capacity :initarg :capacity :initform 0)))

(defclass node()
  ((name :initarg :name)
   (color :initarg :color :initform :white)
   (neighbours :initarg :adj :initform '())))

(defclass graph()
  ((nodes :initarg :nodes)
   (edges :initarg :edges)))

(defun graph-edges(g)
  (slot-value g 'edges))

(defun graph-find-edge(v1 v2 g)
  (dolist (edge (graph-edges g))
    (if (and (string=  (slot-value edge 'start) (node-name v1))
             (string= (slot-value edge 'end)   (node-name v2)))
        (return edge))))

(defun graph-nodes(g)
  (slot-value g 'nodes))


(defun find-node(name nodes)
  (loop 
     for node in nodes 
     for k = nil do 
     (if (string= name (slot-value node 'name))
         (progn
           (return node)))))




(defun node-name(node)
  (slot-value node 'name))

(defun node-color(node)
  (slot-value node 'color))

(defun node-colorp(n c)
  (eql (node-color n) c))

(defun node-names(nodes)
  (loop 
    for node in nodes
    collect (slot-value node 'name)))

(defun node-neighbours(node)
  (slot-value node 'neighbours))

(defun node-count(g)
  (length (graph-nodes g)))

(defun node-not-terminal(node)
  (> (length (node-neighbours node)) 0))

(defun node-has-neighbourp(node neighbour)  
  (position (node-name neighbour) (node-neighbours node) :test #'string= :key #'node-name))

(defun node-add-neighbour(node neighbour)
  (if (not (node-has-neighbourp node neighbour))
        (push neighbour (slot-value node 'neighbours))))

(defun edge-count(g)
  (length (graph-edges g)))

(defun edge-capacity(v1 v2 g)
  (let ((edge (graph-find-edge v1 v2 g)))
    (if edge
        (slot-value edge 'capacity))))

(defun edge-flow-to-capacity(v1 v2 g)
  (>= (- (edge-capacity v1 v2  g ) (edge-flow v1 v2 g)) 0))

(defun edge-flow(v1 v2 g)
  (let ((edge (graph-find-edge v1 v2 g)))
    (if edge
        (slot-value edge 'flow))))

(defun edges->node-neighbours(n edges)
  (let ((adjv '()))
    (loop 
       for e in edges do
         (if (string= n (slot-value e 'start))
               (push (slot-value e 'end) adjv)))
    (remove-duplicates adjv :test #'string=)))


(defun bfs-enqueue(n q)
  (push n q)
  (setf (slot-value n 'color) :gray) q)

(defun bfs-dequeue(q)
  (let ((n (pop q)))
    (setf (slot-value n 'color) :black) n))

(defun bfs(start end graph)
  (loop for node in (graph-nodes graph)
        do 
        (setf (slot-value node 'color) :white))
  (loop with nodes = (graph-nodes graph)
        for node in nodes
        with start_node = (find-node start nodes)
        with end_node   = (find-node end nodes)
        with queue = (bfs-enqueue start_node '())

        for cur = (bfs-dequeue queue)        
        for path = (list cur) then (push cur path)
        while (and (> (length queue) 0)
                  (not (string= (node-name cur) end)))
        finally (return (nreverse path))
        do   
;;      (format t "queue [~a] ~%" (mapcar #'node-name queue))
        (loop for neighbour  in (node-neighbours cur)
              when (and 
                    (node-colorp neighbour :white)
                    (edge-flow-to-capacity cur neighbour graph))
              do 
              (setf queue (bfs-enqueue neighbour queue))
              ;; (format t "Found path [~a]->[~a] ~%" 
              ;;         (node-name cur)
              ;;         (node-name neighbour))
              ;;(push neighbour path)
)))

(defun path->string(path)
  (format nil "~{~A~^->~}" (node-names path)))

(defun path-flow-increment(path inc g)
  (loop for ls = path then (cdr ls)
        for (v1 v2) = (take 2 ls)
        for e = (graph-find-edge v1 v2 g)
;;PATH-FLOW-INCREMENT        while (and v1 v2 )
   do 
   (format t "Flow  ~a ~a  ~%" (node-name v1) (node-name v2))
   (incf (slot-value e 'flow) inc)))



(defun edges->nodes(edges)
  (let ((nodes '())
        (cur_node '()))    
    (loop 
       for edge in edges 
       for end_vertex_name    = (slot-value edge 'end)
       for start_vertex_name  =  (slot-value edge 'start)
       for cur_node = (find-node start_vertex_name nodes)
       for end_node = (find-node end_vertex_name nodes)
      do
         (if (not cur_node)
             (progn
               (setq cur_node (make-instance 'node :name start_vertex_name))
               (push cur_node nodes)))         
         (if (not end_node)
             (progn
               (setq end_node (make-instance 'node :name end_vertex_name))
               (push end_node nodes)))
         (progn
           (node-add-neighbour cur_node end_node)))
           nodes))

(defun edges->node-names(edges)
  (let ((node-names '()))
    (dolist (e edges)
      (push (slot-value e 'start) node-names)
      (push (slot-value e 'end) node-names))
    (setq node-names (remove-duplicates node-names :test #'string= ))))

(defun read-edge-file(fn)
  (let ((in (open fn :if-does-not-exist nil))
        (edges '()))
    (when in
      (loop for line = (read-line in nil)
         while line do 
           (let ((l (split-by-one-space line)))
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

(defun parse-graph(fn)
  (let* ((edges (read-edge-file fn))
         (nodes (edges->nodes edges)))
    (make-instance 'graph 
                   :nodes nodes
                   :edges edges)))

(defparameter *g* (parse-graph *tg*))
(defparameter es (graph-edges *g*))

(defun print-edge(edge)
  (if edge 
      (format t "(~a)->(~a) [~a] ~%" 
              (slot-value edge 'start) 
              (slot-value edge 'end) 
              (slot-value edge 'capacity))))

(defun print-graph-edges(g)
  (mapcar #'print-edge (graph-edges g)))
