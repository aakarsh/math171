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
   (edges :initarg :edges)
   (flow :initarg :flow)))

(defun graph-edges(g)
  (slot-value g 'edges))


(defun graph-find-edge(v1 v2 g)
  (dolist (edge (graph-edges g))
    (if (and (string=  (slot-value edge 'start) (node-name v1))
             (string= (slot-value edge 'end)   (node-name v2)))
        (return edge))))

(defun graph-nodes(g)
  (slot-value g 'nodes))

(defun node-name->index(name g)
  (position name (graph-nodes g) 
            :test #'string= :key #'node-name))

(defun node->index(node g)
  (node-name->index (node-name node) g))


(defun find-node(name nodes)
  (loop 
     for node in nodes 
     for k = nil do 
     (if (string= name (slot-value node 'name))
         (progn
           (return node)))))

(defun node-name(node)
  (if node
      (slot-value node 'name)))

(defun node-color(node)
  (slot-value node 'color))

(defun node-colorp(n c)
  (eql (node-color n) c))

(defun node-names(nodes)
  (loop 
    for node in nodes
    when node
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


(defun edge-start(e)  
  (slot-value e 'start))

(defun edge-end(e)
  (slot-value e 'end))

(defun edge-capacity(v1 v2 g)
  (let ((edge (graph-find-edge v1 v2 g)))
    (if edge
        (slot-value edge 'capacity))))

(defun graph-flow(g)
  (slot-value g 'flow))

(defun edge-flow(v1 v2 g)
  (let ((flow (graph-flow g))
         (i1  (node->index v1 g))
         (i2  (node->index v2 g)))
    (aref flow i1 i2)))

(defun edge->flow(e g)
  (edge-flow (edge-start e) (edge-end e)g))

(defun edge-flow-inc(v1 v2 inc g)
  (incf (aref (graph-flow g) 
              (node-name->index v1 g) 
              (node-name->index v2 g)) inc))

(defun edge-flow-increment(e inc g)
  (edge-print e)
  (incf (aref (graph-flow g)
               (node-name->index(edge-start e) g)
               (node-name->index (edge-end e) g)) inc))

(defun edge-available-capacity(e g)
  (- (slot-value e 'capacity) (edge->flow e g)))

(defun edge-flow-to-capacity(v1 v2 g)
  (>= (- (edge-capacity v1 v2  g ) (edge-flow v1 v2 g)) 0))


(defun edges->node-neighbours(n edges)
  (let ((adjv '()))
    (loop 
       for e in edges do
         (if (string= n (slot-value e 'start))
               (push (slot-value e 'end) adjv)))
    (remove-duplicates adjv :test #'string=)))

(defun edges-funcall(g f)
  (loop for e in (graph-edges)
        do 
        (funcall f e)))

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
        with debug = nil
        with start_node = (find-node start nodes)
        with end_node   = (find-node end nodes)
        with queue = (bfs-enqueue start_node '())
        for cur = (bfs-dequeue queue)
        with path = (list start_node) 
        while (and (> (length queue) 0)
                  (not (string= (node-name cur) end))) 
        finally (if (not (string= (node-name cur) end))
                    nil
                  (progn                    
                    (push cur path)   
                    (delete-duplicates path :key #'node-name :test #'string=)
                    (return (nreverse path))))
        do   
        (if debug
            (format t "Queue [~a] ~%" (mapcar #'node-name queue)))
        (loop for neighbour  in (node-neighbours cur)
              when (and 
                    (node-colorp neighbour :white)
                    (edge-flow-to-capacity cur neighbour graph))
              do 
              (if debug
                  (format t "Found path [~a]->[~a] ~%" 
                          (node-name cur) 
                          (node-name neighbour)))
              (setf queue (bfs-enqueue neighbour queue))
              (push cur path))))


;;path-flow-increment while (and v1 v2 )

(defun path->string(path)
  (format nil "~{~A~^->~}" (node-names path)))


(defun path->edges(path g f)
  (loop for ls = path then (cdr ls)
        for (v1 v2) = (take 2 ls)
        for e = (graph-find-edge v1 v2 g)
        while (and v1 v2)
        do
        (if e
            (funcall f e))))

(defun test-path->edges()
  (path->edges (bfs "s" "t" *g*)  *g* #'edge-print))

(defun edge-print(e )
  (format t "(~a)->(~a) [c:~a] [f:??] <??>~%" 
            (slot-value e 'start) 
            (slot-value e 'end) 
            (slot-value e 'capacity)  
;;            (edge->flow e)
;;            (edge-available-capacity e)
))

;; (defun edge-increment-flow(e inc)
;;      (edge-print e)
;;      (incf (slot-value e 'flow) inc))

(defun path-flow-increment(path inc g )
  (path->edges path  g 
               #'(lambda(e) (edge-flow-increment e inc g))))

(defvar *max-increment* 1000000000)

(defun find-path-increment(path g)
  (let* ((min *max-increment*)
         (cur-min min))
    (path->edges path g 
          #'(lambda(e)                     
              (setf cur-min (edge-available-capacity e g))
              (if (< cur-min min)
                  (setf min cur-min)))) min))

(defun max-flow(start end g)
  (loop edge in (graph-edges g)
        do 
        (setf (slot-value edge 'flow) 0))  
  (loop path = (bfs start end g)
        while path
        do 
        (path-flow-increment path (find-path-increment path g) g)))

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
           (sort nodes  #'string< :key #'node-name )))

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
                   :edges edges
                   :flow (make-array (list (length nodes) (length nodes))))))

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
