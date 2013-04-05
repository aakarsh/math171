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

(defun str/split-by-one-space (str)
    "Returns a list of substrs of str divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
str between them."
    (when (and str (> (length str) 0))
    (loop for i = 0 then (1+ j)
          as j = (position #\Space str :start i)
          collect (subseq str i j)
          while j)))

(defun str/join-words(words &optional (sep " "))
  (loop for word in words
        for result = word then (concatenate 'string result sep word)
       finally (return result)))


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

(defun node-equals (n1 n2)
  (string= (node-name n1) (node-name n2) ))

(defun node-set-color (node color)
  (setf (slot-value node 'color) color))

(defun node-set-white (node)
  (node-set-color node :white))

(defun node-set-gray (node)
  (node-set-color node :gray))

(defun node-set-black (node)
  (node-set-color node :black))

(defun nodes-set-white (nodes)
  (mapcar #'node-set-white nodes))

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
  (let ((flow (graph-flow g))
         (i1  (node-name->index (edge-start e) g))
         (i2  (node-name->index (edge-end e) g)))
    (aref flow i1 i2)))

(defun edge-flow-inc(v1 v2 inc g)
  (incf (aref (graph-flow g) 
              (node-name->index v1 g) 
              (node-name->index v2 g)) inc))

(defun graph-flow-setf(v1 v2 i g)
  (setf (aref (graph-flow g) 
              (node-name->index v1 g) 
              (node-name->index v2 g)) i))

(defun node-pair-flow-increment(v1 v2 inc g)
 (incf (aref (graph-flow g)
               (node->index v1 g)
               (node->index v2 g)) inc))

(defun edge-flow-increment(e inc g)
  (edge-print e g)
  (incf (aref (graph-flow g)
               (node-name->index(edge-start e) g)
               (node-name->index (edge-end e) g)) inc))

(defun edge->string(e g)
  (format nil "(~a)->(~a) [c:~a] [f:~a] <~a>" 
            (slot-value e 'start) 
            (slot-value e 'end) 
            (slot-value e 'capacity)  
            (edge->flow e g)
            (edge-available-capacity e g)))

(defun edge-print(e g)
  (format t "~a~%"  (edge->string e g)))

(defun edge-available-capacity(e g)
  (- (slot-value e 'capacity) (edge->flow e g)))

(defun edge-flow-to-capacity(v1 v2 g)
  (> (- (edge-capacity v1 v2  g ) (edge-flow v1 v2 g)) 0))

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

(defmacro bfs-enqueue!(n q)
  `(progn 
     (if ,n 
         (progn 
           (node-set-gray ,n)
           (push ,n ,q)))
     ,q))

(defmacro bfs-dequeue!(q)
  `(let ((n (pop ,q)))
     (if n
         (node-set-black n))
     n))


(defmacro push-node-and-predecessor!(node prev path)
  `(progn      
     (push  (list ,node ,prev) ,path)
     (if debug
         (format t "Pushed (~a ~a) -> [~a] ~%" 
                 (node-name ,node) 
                 (node-name ,prev)
                 (bfs-pred-path->string ,path)))
     ,path))


(defun reconstruct-path(pred_list start end)  
  (loop with path = '() 
        for prev = end then (cadr pair)
        for pair = (assoc (node-name prev) pred_list :key #'node-name :test #'string=)
        for cur =  (car pair)
        while cur
        with debug = nil
        finally (return path)        
        do
        (if debug (format t "cur:~a,prev:~a ~%" cur prev))
        (push cur path)))

(defun bfs-pred-path->string(path)
  (loop for elem in path
        for node = (car elem)
        for pred = (cadr elem)        
        for result = (str/join-words (list result           
                                           (format nil "(~a ~a)"  (node-name node)  (node-name pred))))
        finally (return result)))


(defun node-not-equals (n1 n2)
  (not (node-equals n1 n2)))

(defun bfs(start end graph)
  (let* ((nodes (graph-nodes graph))
        (start_node (find-node start nodes))
        (end_node   (find-node end nodes))
        (queue      '())
        (path        (list (list start_node nil))))
    (nodes-set-white nodes)    
    (bfs-enqueue! start_node queue)
    (loop for node in nodes
          with debug = nil
          for prev = nil then cur_node
          for cur_node = (bfs-dequeue! queue)
          while  (and cur_node (node-not-equals cur_node end_node)) 
          finally (if (node-not-equals cur_node end_node)
                      (progn 
                        (if debug
                            (format t "Returning on ~a Queue ~a " (node-name cur_node ) queue))
                        nil)
                    (progn                    
                      (if prev 
                          (push-node-and-predecessor! cur_node prev path))                      
                      (return (reconstruct-path path start_node end_node))))
          do           
          (if debug (format t "~%cur_node[~a] bfs-queue[~a] ~%neighbours:~%~a ~%" 
                            (node-name cur_node) 
                            (node-names queue)
                            (mapcar #'(lambda(neighbour) 
                                        (format nil "[neighbour [~a] c:~a f:~a accessible:~a ]"  
                                                (node-name neighbour)
                                                (edge-capacity cur_node neighbour  graph)
                                                (edge-flow cur_node neighbour  graph)
                                                (edge-flow-to-capacity cur_node neighbour  graph)))
                                    (node-neighbours cur_node))))

          (loop for neighbour  in (node-neighbours cur_node)
                do 
                (if (and 
                     (node-colorp neighbour :white)
                     (edge-flow-to-capacity cur_node neighbour graph))
                    
                    (progn
                      (if debug (format t "~%Enqueue Neighbour: [~a] <c:~a> <f:~a> accessible:~a " 
                                        (node-name neighbour) 
                                        (edge-capacity cur_node neighbour graph) 
                                        (edge-flow cur_node neighbour graph)
                                        (edge-flow-to-capacity cur_node neighbour  graph)))                      

                      (bfs-enqueue! neighbour queue)
                      (format t "bfs-queue[~a] ~%" (node-names queue))
                      (push-node-and-predecessor! neighbour cur_node  path)
))))))




(defun path->string(path)
  (format nil "~{~A~^->~}" (node-names path)))

(defun path->on_vertex_pair(path g f)
  (loop for ls = path then (cdr ls)
        for (v1 v2) = (take 2 ls)
        while (and v1 v2)
        do
        (if (and v1 v2)
            (funcall f v1 v2))))

(defun path->edges(path g f)
  (loop for ls = path then (cdr ls)
        for (v1 v2) = (take 2 ls)
        for e = (graph-find-edge v1 v2 g)
        while (and v1 v2)
        do
        (if e
            (funcall f e))))

(defun path-flow-increment(path inc g )
  (path->edges path  g 
               #'(lambda(e) 
                   (format t "Before ~a ~%" (edge->string e g))
                   (edge-flow-increment e inc g)
                   (format t "After  ~a ~%" (edge->string e g))))  
  (path->on_vertex_pair (reverse path) g
             #'(lambda(v1 v2) 
                 (node-pair-flow-increment v1 v2 (- inc) g))))


(defvar *max-increment* 1000000000)

(defun find-path-increment(path g)
  (let* ((min *max-increment*)
         (cur-min min))
    (path->edges path g 
          #'(lambda(e)                     
              (setf cur-min (edge-available-capacity e g))
              (if (< cur-min min)
                  (setf min cur-min)))) 
    min))

(defun node-list-neighbours(node-name g)
  (node-names (node-neighbours (find-node node-name (graph-nodes g)))))

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
           (let ((l (str/split-by-one-space line)))
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


(defun print-edge(edge)
  (if edge 
      (format t "(~a)->(~a) [~a] ~%" 
              (slot-value edge 'start) 
              (slot-value edge 'end) 
              (slot-value edge 'capacity))))

(defun print-graph-edges(g)
  (mapcar #'print-edge (graph-edges g)))

(defun max-flow(start end g)
  (loop for e in (graph-edges g)
        do 
        (graph-flow-setf (edge-start e) (edge-end e) 0 g))
   (loop for path = (bfs start end g)
         while path
         for increment =  (find-path-increment path g)
         for max_flow = increment then (+ max_flow increment)
         finally (return max_flow)
         do 
         (format t "Path ~a increment ~a ~%" (path->string path) increment)
         (path-flow-increment path increment g)))


(defparameter *g* (parse-graph *tg*))
(defparameter es (graph-edges *g*))

(defun test-max-flow ()
  (eql 23 (max-flow "s" "t" *g*)))
