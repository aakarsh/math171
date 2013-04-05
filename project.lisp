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

(defun make-edge (start end capacity)
  (make-instance 'edge :start-node start  :end-node end :capacity capacity))

(defun make-edge-string-list(ls)
  (make-edge (first ls ) (second ls) (parse-integer (third ls))))


(defclass node()
  ((name :initarg :name)
   (color :initarg :color :initform :white)
   (neighbours :initarg :adj :initform '())))

(defclass node-neighbour ()
  ((node :initarg :node)
   (capacity :initarg :capacity :initform 0)
   (flow :initarg :flow :initform 0)))



(defclass graph()
  ((nodes :initarg :nodes)
   (edges :initarg :edges)
   (flow :initarg :flow)))

(defclass course-data ()
  ((name :initarg :course-name)
   (title :initarg :course-title)
   (section :initarg :course-section)
   (code :initarg :course-code)
   (type :initarg :course-type)
   (notes :initarg :course-notes)
   (capacity :initarg :course-capacity)
   (dates :initarg :course-dates)
   (location :initarg :course-location)
   (professor :initarg :course-professor)))


(defun make-graph-simple (nodes edges)
  (make-instance 'graph 
                 :nodes nodes
                 :edges edges
                 :flow (make-flow (length nodes))))

(defun make-empty-node (name)
 (make-instance 'node :name name))

(defun nodes->edges (g)
  
)
(defun make-flow (n)
  (make-array (list n n)))

(defun graph-edges(g)
  (slot-value g 'edges))


(defun graph-find-edge(v1 v2 g)
  (dolist (edge (graph-edges g))
    (if (and (string=  (edge-start edge) (node-name v1))
             (string= (edge-end edge)  (node-name v2)))
        (return edge))))

(defun graph-nodes(g)
  (slot-value g 'nodes))

(defun node-index->node (i g)
  (nth i (graph-nodes g)))

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

;; (defun edge-count(g)
;;   (length (graph-edges g)))

(defun edge-start(e)  
  (slot-value e 'start))

(defun edge-end(e)
  (slot-value e 'end))

(defun edge-capacity1 ( e)
  (slot-value e 'capacity))

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

(defmacro make-node-if-none (node_name nodes)
  (let ((n (gensym "n"))) 
    `(let ((,n (find-node ,node_name ,nodes)))
       (if (not ,n)
           (progn
             (setq ,n (make-empty-node ,node_name))
             (push ,n ,nodes)))
       ,n)))


(defun add-nodes-from-edges(edges nodes)
  (loop for edge in edges 
        for cur_node = (make-node-if-none (edge-start edge) nodes)
        for end_node = (make-node-if-none (edge-end edge) nodes) 
        finally (return (sort nodes  #'string< :key #'node-name))
        do
        (node-add-neighbour cur_node end_node)))

(defun edges->nodes(edges)
  (let ((nodes '()))
    (add-nodes-from-edges edges nodes)))


(defun edges->node-names(edges)
  (let ((node-names '()))
    (dolist (e edges)
      (push (edge-start e) node-names)
      (push (edge-end e) node-names))
    (remove-duplicates node-names :test #'string= )))


(defun read-edge-file(file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (edges '()))
    (when in
      (loop for line = (read-line in nil)
         while line do 
           (let ((l (str/split-by-one-space line)))
             (when (eql (length l) 3)
               (push (make-edge-string-list l) edges)))))
    (close in)
    (nreverse edges)))

(defun parse-graph(file-name)
  (let* ((edges (read-edge-file file-name))
         (nodes (edges->nodes edges)))
    (make-graph-simple nodes edges)))

(defun make-edges-from-pairs (pair-list)
  (loop for (v1 v2) in pair-list
        collect (make-edge v1 v2 1)))

(defun pair-list->matching-graph (pairs &optional (source_name "Source") (sink_name "Sink"))
  (let* ((edges  (make-edges-from-pairs pairs))
         (nodes (edges->nodes edges))
         (new-edges '()))    
    (loop for edge in edges
          for l = (edge-start edge)
          for r = (edge-end edge)
          do 
          (push (make-edge  source_name l 1) new-edges)
          (push (make-edge  r sink_name 1) new-edges))
    (setq nodes (add-nodes-from-edges new-edges nodes))
    (make-graph-simple nodes (concatenate 'list edges new-edges))))




(defun print-edge(edge)
  (if edge 
      (format t "(~a)->(~a) [~a] ~%" 
              (edge-start edge)
              (edge-end edge)
              (edge-capacity1 edge))))


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

(defun maximal-matching (pair-list)
  (let* ((matching-graph (pair-list->matching-graph pair-list))
        (match-size 0)
        (nodes (graph-nodes matching-graph))
        (num_nodes (length nodes))
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





(defun test-maximal-matching ()
  (eql 6 (length  (maximal-matching '(("A" "d") ("A" "h") ("A" "t")
                                      ("B" "g")  ("B" "p") ("B" "t")
                                      ("C" "a")   ("C" "g")   ("C" "h")  
                                      ("D" "h")   ("D" "p")   ("D" "t")  
                                      ("E" "a")   ("E" "c")   ("E" "d")  
                                      ("F" "c")   ("F" "d")   ("F" "p"))))))

;;(test-maximal-matching)

