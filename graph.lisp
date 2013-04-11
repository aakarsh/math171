(in-package :com.bovinasancta.graph)

(defclass graph()
  ((nodes :initarg :nodes :accessor graph-nodes)
   (flow :initarg :flow :accessor graph-flow)))

(defclass node()
  ((name :initarg :name )
   (color :initarg :color :initform :white :accessor node-color)
   (neighbours :initarg :adj :initform '() :accessor node-neighbours)
   (node-edges :initarg :adj :initform '() :accessor node-edges)))

(defclass node-edge ()
  ((node :initarg :node :accessor node-edge-node )
   (capacity :initarg :capacity :initform 0 :accessor node-edge-capacity )))

(defun node-name(node)
  (if node
      (slot-value node 'name)))

(defun node-find-edge (node neighbour)
  (loop for ne in (node-edges node)
        when (eql ne neighbour)
        do
         (return ne)))

(defun node-find-node-edge-by-name (node neighbour-name)
  (loop for ne in (node-edges node)
        for ne-node = (slot-value ne 'node)       
        when (string= (node-name ne-node) neighbour-name)
        do
         (return ne)))

(defun make-graph-simple (nodes edges)
  (make-instance 'graph 
                 :nodes nodes
                 :flow (make-flow (length nodes))))

(defun make-empty-node (name)
 (make-instance 'node :name name))

(defun make-2d-array (n)
  (make-array (list n n) ))

(defun make-flow (n)
  (make-2d-array n))

(defun graph-set-flow-zero (g)
  (setf (graph-flow g) (make-flow (length (graph-nodes g)))))

(defun node-index->node (i g)
  (nth i (graph-nodes g)))

(defun node-name->index(name g)
  (position name (graph-nodes g) 
            :test #'string= :key #'node-name))

(defun node->index(node g)
  (node-name->index (node-name node) g))

(defun find-node(name nodes)
  (loop for node in nodes 
     do 
     (if (string= name (node-name node))
         (return node))))

(defun find-node-in-graph (name g)
  (find-node name (graph-nodes g)))

(defun node-color=(n c)
  (eql (node-color n) c))

(defun node-equals (n1 n2)
  (string= (node-name n1) (node-name n2) ))

(defun node-set-white (node)
  (setf (node-color node) :white))

(defun node-set-gray (node)
  (setf (node-color node) :gray))

(defun node-set-black (node)
  (setf (node-color node) :black))

(defun nodes-set-white (nodes)
  (mapcar #'node-set-white nodes))

(defun node-names(nodes)
  (loop for node in nodes
     when node
     collect (node-name node)))

(defun node-count(g)
  (length (graph-nodes g)))


(defun node-not-terminal(node)
  (> (length (node-neighbours node)) 0))

(defun node-has-neighbourp(node neighbour)  
  (position (node-name neighbour) (node-neighbours node)
            :test #'string= :key #'node-name))

(defun edge-capacity (v1 v2)
  (loop for ne in  (node-edges v1)
     do
       (if (string= (node-name v2) (node-name (node-edge-node ne)))
           (return  (node-edge-capacity ne)))))

(defun edge-flow(v1 v2 g)
  (let ((flow (graph-flow g))
         (i1  (node->index v1 g))
         (i2  (node->index v2 g)))
    (aref flow i1 i2)))

(defun edge-flow-inc(v1 v2 inc g)
  (incf (aref (graph-flow g) 
              (node-name->index v1 g) 
              (node-name->index v2 g)) inc))

(defun node-pair-flow-increment(v1 v2 inc g)
 (incf (aref (graph-flow g)
               (node->index v1 g)
               (node->index v2 g)) inc))


(defun node-edge-capacity-1 (v1 v2 g)
  (let ((ne  (node-find-node-edge-by-name (find-node-in-graph (node-name v1) g) (node-name v2))))
    (node-edge-capacity ne)))

(defun available-capacity (v1 v2 g)
  (- (node-edge-capacity-1 v1 v2 g)  ( edge-flow v1 v2 g)))

(defun edge-flow-to-capacity(v1 v2 g)
  (> (- (edge-capacity v1 v2 ) (edge-flow v1 v2 g)) 0))

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
     (if nil
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
                                           (format nil "(~a ~a)"  
                                                   (node-name node)  
                                                   (node-name pred))))
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
                            (format t "Returning on ~a Queue ~a " 
                                    (node-name cur_node ) queue))
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
                                                (edge-capacity cur_node neighbour)
                                                (edge-flow cur_node neighbour  graph)
                                                (edge-flow-to-capacity cur_node neighbour  graph)))
                                    (node-neighbours cur_node))))

          (loop for neighbour  in (node-neighbours cur_node)
                do 
                (if (and 
                     (node-color= neighbour :white)
                     (edge-flow-to-capacity cur_node neighbour graph))                    
                    (progn
                      (if debug (format t "~%Enqueue Neighbour: [~a] <c:~a> <f:~a> accessible:~a " 
                                        (node-name neighbour) 
                                        (edge-capacity cur_node neighbour) 
                                        (edge-flow cur_node neighbour graph)
                                        (edge-flow-to-capacity cur_node neighbour  graph)))                      
                      (bfs-enqueue! neighbour queue)
                      (if debug (format t "bfs-queue[~a] ~%" (node-names queue)))
                      (push-node-and-predecessor! neighbour cur_node  path)))))))

(defun path->string(path)
  (format nil "~{~A~^->~}" (node-names path)))

(defun path->on_vertex_pair(path g f)
  (loop for ls = path then (cdr ls)
        for (v1 v2) = (take 2 ls)
        while (and v1 v2)
        do
        (if (and v1 v2)
            (funcall f v1 v2))))

(defun path->node-edges(path g f)
  (loop for ls = path then (cdr ls)
        for (v1 v2) = (take 2 ls)
        for e = (node-find-edge v1 v2 )
        while (and v1 v2)
        do
        (if e
            (funcall f e))))

(defun path-flow-increment(path inc g )
  (path->on_vertex_pair path g
               #'(lambda(v1 v2) 
                   (node-pair-flow-increment v1 v2 inc g)))
  (path->on_vertex_pair (reverse path) g
             #'(lambda(v1 v2) 
                 (node-pair-flow-increment v1 v2 (- inc) g))))

(defvar *max-increment* 1000000000)

(defun find-path-increment(path g)
  (let* ((min *max-increment*)
         (cur-min min))
    (path->on_vertex_pair path g
                          #'(lambda (v1 v2)
                              (setf cur-min (available-capacity v1 v2 g))
                              (if (< cur-min min)
                                  (setf min cur-min))))    
    min))

(defun node-list-neighbours(node-name g)
  (node-names
   (node-neighbours
    (find-node-in-graph node-name g))))

(defmacro make-node-if-none (node_name nodes)
  (let ((n (gensym "n"))) 
    `(let ((,n (find-node ,node_name ,nodes)))
       (if (not ,n)
           (progn
             (setq ,n (make-empty-node ,node_name))
             (push ,n ,nodes)))
       ,n)))

(defun sort-nodes (nodes)
  (sort nodes  #'string< :key #'node-name))

(defun node-add-neighbour(node neighbour &optional (capacity 1) (flow 0))
  (if (not (node-has-neighbourp node neighbour))
      (progn 
        (push neighbour (node-neighbours node))
        (push (make-instance 'node-edge
                             :node neighbour
                             :capacity capacity)
              
              (node-edges node)))))


(defun edges->nodes-conditionally(edges nodes)
  (loop
     for edge in edges 
     for cur_node = (make-node-if-none (first edge) nodes)
     for end_node = (make-node-if-none (second edge) nodes) 
     for capacity = (third edge)
     finally (return (sort-nodes nodes))
        do
       (node-add-neighbour cur_node end_node capacity)))

(defun edges->nodes(edges)
  (edges->nodes-conditionally edges '()))

(defun string->edge (line)
  (let ((ls (str/split-by-one-space line)))
    (when (eql (length ls) 3)
      (list (first ls) (second ls) (parse-integer (third ls))))))

(defun read-edge-file(file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (edges '()))
    (when in
      (loop for line = (read-line in nil)
         while line
         for edge = (string->edge line)
         when edge
         do  (push edge edges))      
      (close in))
    (nreverse edges)))

(defun parse-graph(file-name)
  (let* ((edges (read-edge-file file-name))
         (nodes (edges->nodes edges)))
    (make-graph-simple nodes edges)))

(defun pairs->edges (pair-list)
  (loop for (v1 v2) in pair-list
        collect (list v1 v2 1)))

(defun pair-list->matching-graph (pairs &optional
                                          (source_name "Source")
                                          (sink_name "Sink"))
  (let* ((edges  (pairs->edges pairs))
         (nodes (edges->nodes edges))
         (new-edges '()))    
    (loop for edge in edges
          for l = (first edge)
          for r = (second edge)
          do 
          (push (list source_name l 1) new-edges)
          (push (list  r sink_name 1) new-edges))
    (setq nodes (edges->nodes-conditionally new-edges nodes))
    (make-graph-simple nodes (concatenate 'list edges new-edges))))

(defun max-flow(start end g)
  (graph-set-flow-zero g)
   (loop for path = (bfs start end g)
         while path
         for increment =  (find-path-increment path g)
         for max_flow = increment then (+ max_flow increment)
         finally (return max_flow)
         do 
         (if nil (format t "Path ~a increment ~a ~%" (path->string path) increment))
         (path-flow-increment path increment g)))

(defun maximal-matching (pair-list)
  (let* ((matching-graph (pair-list->matching-graph pair-list))
        (match-size 0)
        (nodes (graph-nodes matching-graph))
        (matching '()))
    (setq match-size (max-flow "Source" "Sink" matching-graph))    
    (loop for n1 in nodes
          do 
          (loop for n2 in nodes
                 do
                 (if (and (not (string= "Source" (node-name n1)))
                          (not (string= "Sink" (node-name n2)))
                          (>  (edge-flow n1 n2 matching-graph) 0))                     
                     (push (list (node-name n1) (node-name n2))  matching))))
    matching))

(defparameter *sample-graph-file*  "sample-graph.lisp")
(defparameter *tg* "simple.txt")

(defparameter *g* (parse-graph *tg*))

(defun test-num-nodes()
  (eql 6 (node-count *g*)))

(defun test-max-flow()
  (eql 23 (max-flow "s" "t" *g*)))

(defparameter *max-matching-test-data*
  '(("A" "d") ("A" "h") ("A" "t")
    ("B" "g")  ("B" "p") ("B" "t")
    ("C" "a")   ("C" "g")   ("C" "h")  
    ("D" "h")   ("D" "p")   ("D" "t")  
    ("E" "a")   ("E" "c")   ("E" "d")  
    ("F" "c")   ("F" "d")   ("F" "p")))

(defun test-maximal-matching ()
  (eql 6 (length  (maximal-matching *max-matching-test-data* ))))


(if (not (and
          (test-num-nodes)
          (test-maximal-matching)
          (test-max-flow)))
    (error "Failing tests"))

