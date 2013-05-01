(in-package :com.bovinasancta.project)

(load "data.lisp")

(defparameter *pt-faculty* 
           '("s ahmed" "s arabhi"
             "m bodas" "s desousa"
             "c fan"   "t fish"
             "j hilliard" "t huynh"
             "k jensen" "a jiru"
             "j jordan" "o kovaleva"
             "r low"    "j lum"
             "w newball" "a nguyen"
             "t nguyen"  "v nguyen"
             "l papay" "p rogers"
             "l roper" "l sega"
             "t smith" "a strong"
             "a talebi" "p tanniru"
             "a tran"   "q tran"
             "j trubey" "m van-der-poel"
             "m vartanian" "s vergara"
             "j wang" "e zabric"
             "m zoubeidi")           
           "List of part time faculty")

(defparameter *prof-faculty*
          '("j becker" "m blockus" "r dodd" "l foster"
            "t hsu" "k kellum" "r kubelka" "h ng"
            "s obaid" "b pence" "b peterson" "f rivera"
            "m saleem" "e schmeichel" "w so" "m stanley" 
            "c roddick" "sliva-spitzer"   "r pfiefer"
            "h katsuura"  "m beeson"
;;            professors without any classes are assumed to be on leave
            "shubin"
            "jackson"
;;            "alperin"
            )
          "List of full time faculty")

(defparameter *ass-faculty*
           '( "s crunk" "a gottlieb" "p koev" "b lee"
              "j maruskin" "s simic"  "m cayco-gajic"
              #|"bremer"|#  )
           "List of Associate or Assistant Professors")

(+ (length *ass-faculty*) (length *pt-faculty*))

(defclass time-interval ()
  ((start  :initarg :start :initform 0 :accessor interval-start)
   (end  :initarg :end :initform 0  :accessor interval-end)))


(defun string->time-interval (str)
  (if (and  str (> (length str) 0) (not (string=  "tba" str)) )
      (destructuring-bind (start end)
          (mapcar #'parse-integer (split-by-char str #\-))
        (make-instance 'time-interval :start start :end end))))

(assert (eql 900 (interval-start (string->time-interval "0900-1000"))))
(assert (eql 1000 (interval-end (string->time-interval "0900-1000"))))

(defclass name()
  ((first-name :initarg :first :accessor first-name)
   (last-name  :initarg :last  :accessor last-name)))

(defun string->name (name)
  (if name
      (destructuring-bind (first-name  &optional (last-name nil) &rest args) (str/split-by-one-space name)
        (if (not last-name)
            (progn
              (setq last-name first-name)
              (setq first-name "")))
        (make-instance 'name :first first-name :last last-name))))

(defun name->string (name)
  (if (not name)
      ""  
    (let ((fname (first-name name))
          (lname (last-name name)))
      (if (not fname)
          (setq fname ""))
      (if (not lname)
          (setq lname ""))
      (str/concat fname " " lname))))

(assert ( string= "foo bar" (name->string (string->name "foo bar"))))
(assert (string= "foo" (first-name (string->name "foo bar"))))
(assert (string= "" (first-name (string->name "bar"))))
(assert (string= "bar" (last-name (string->name "foo bar"))))

(define-default-class course (name title section code credit type time
                              notes capacity dates days location professor))

(defmethod initialize-instance :after ((obj course)  &key professor time   &allow-other-keys)
  (if nil
      (format t "Before :Creating course-data name [~a] time[~a] ~%" professor time ))
  (setf (slot-value obj 'professor)  (string->name professor))  
  (setf (slot-value obj 'time) (string->time-interval time)))

(defun professor-map->names(map)
  (hash-keys *professor-map*))

(defun professor-coures-map (courses)
  (loop for course in courses
        for prof = (name->string (course-professor course))
        with map = (make-hash-table :test 'equal)
        for map-slot = (gethash prof map)
        while course
        finally (return map)
        do
        (if (not map-slot)
            (progn 
              (setf (gethash prof map) '())))        
        (push course (gethash prof map))))

(defun pre-process-term (term)
  (loop with course-data = '()
     for ls = term then (cddr ls)
     for p1 = (car ls)
     for p2 = (string-trim '(#\Space) (cadr ls))
     for pair = (list p1 p2)
     while (first pair)
     finally (return course-data)
     do 
       (setq course-data (append pair course-data))))

(defun classlist->course-data (class-list)
  (loop for term in class-list
     for course-data = (pre-process-term term)
     with parsed-data = '()
     finally (return parsed-data)
     while term
     do      
       (push (apply #'make-instance 'course course-data) parsed-data)))

(defun read-course-data(file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (parsed-data '()))
    (when in
      (loop for term =  (read in nil)
         for course-data = (pre-process-term term)
         while term
         do      
           (push (apply #'make-instance 'course course-data) parsed-data))      
      (close in))    
    parsed-data))

(defun course-intersectp (course1 course2)
  (and 
   (time-interval-intersects (course-time course1) 
                             (course-time course2))
   (days-intersect  (course-days course1) 
                    (course-days course2))))

(defun interval-contains (a ti)
  (betweenp a (interval-start ti) (interval-end ti)))

(defun days-intersect(d1 d2)
  (list-intersect (string->list d1)
                  (string->list d2)))

(defun professor-find-courses (prof prof_map)
  (gethash prof prof_map))

;; Find a union of all the courses being
;; watched by this professor in the current mappings
;; @input prof1 professor name inputted
;; @input map   complete mapping
;; @input current-mappings mappings chosen
(defun professor-find-watches(prof1 map current-mappings)
  (reduce #'append
          (mapcar #'(lambda(prof2)
                      (professor-find-courses prof2 map))
                  (loop for pair in current-mappings
                        when (string= prof1 (car pair))
                        collect (cadr pair)))))

;; ensure that profesors course commitments and watching commitments dont overlap
(defun professor-overlapping-coursesp (prof1 prof2 map &optional current-mappings)
  (let ((prof1-courses  (union (professor-find-courses prof1 map)
                               (professor-find-watches prof1 map current-mappings)))
        (prof2-courses  (professor-find-courses prof2 map )))
    (loop named outer for course1 in prof1-courses
          do
          (loop for course2 in prof2-courses
                do
                (if (course-intersectp course1 course2)
                    (return-from outer  t))))))

(defun interval->pair (t2)
  (list (interval-start t1) (interval-end t1)))

(defun interval-end-points-in (t1 t2)
  (or
   (interval-contains (interval-start t1) t2)
   (interval-contains (interval-end t1) t2)))

(defun time-interval-intersects (t1 t2)
  (if (or  (not t1) (not t2))
      nil      
    (or 
     (interval-end-points-in t1 t2)
     (interval-end-points-in t2 t1))))

(defun professor-supervisable (prof1 prof2 map &optional current-mappings)
   (not (professor-overlapping-coursesp prof1 prof2 map current-mappings)))

(defun professor-mapping (ls1 ls2 map &optional current-mappings)
  (loop for prof1 in ls1
        with mapping = '()
        finally (return mapping)
        do
        (loop for prof2 in ls2
              do
              (if (professor-supervisable prof1 prof2 map current-mappings )
                  (push (list prof1 prof2) mapping)))))

(defun day-time-overlap(day1 time1 day2 time2 )
  (and  (days-intersect day1 day2) 
        (time-interval-intersects time1 time2)))

(defun print-matching-pairs (matching)
  (loop for pair in matching
        do
       (format t "~a <- ~a ~%" (second  pair) (first  pair))))

(defun print-basic-course-data (course &optional (out t))
;;  (format t "~%")
  (format out "~a   [~a]  [~a - ~a]  "
          (course-title course)          
          (if (slot-boundp  course 'days)
              (course-days course)
            "-")
          (if (and  (slot-boundp course 'time) (course-time course)) 
              (interval-start (course-time course))
            "-")

          (if (and (slot-boundp course 'time) (course-time course))
              (interval-end (course-time course))
            "-"))  
  (format t "~%"))


(defun print-professor-courses (prof map)
  (loop for course in  (professor-find-courses prof map)
        do 
        (print-basic-course-data course)))

(defun match-professor-groups(group1 group2 map &optional current-mappings)
  (let* ((matchable-professors 
            (professor-mapping group1 group2 map current-mappings)))    
    (maximal-matching matchable-professors)))

(defun print-professor-groups(group1 group2 map)
  (let* ((matches (match-professor-groups group1 group2 map)))
    (format t "Matching Size : ~a ~%" (length  matches))
    (print-matching-pairs matches)))

(defparameter *courses*  (classlist->course-data *class-list*))

(defparameter *professor-map* (professor-coures-map *courses*))

(defun sort-matching-by-last-name (matching)
  (sort (copy-list matching) #'string-lessp
        :key  #'(lambda (s)
;;                  (format t "[~a:~a]~%" s (last-name (string->name ( cadr s)))))))
                  (last-name (string->name  (cadr s))))))


;;;Step 1: Take professors and assistants, and match them
(defun prof-ass-matching ()
  (sort-matching-by-last-name
   (match-professor-groups *prof-faculty* *ass-faculty* *professor-map*)))

;;;Step 2: Take assistants, match them to part timers
(defun ass-pt-matching ()
  (sort-matching-by-last-name
   (match-professor-groups *ass-faculty* *pt-faculty* *professor-map*)))

;;;test code
(length  (ass-pt-matching))
;;;Step 3: Take remaining professors, match them to remaining part timers
(defun rem-prof-rem-pt-matching ()
  (sort-matching-by-last-name
   (match-professor-groups 
    (set-difference *prof-faculty*
                    (mapcar #'car (prof-ass-matching)) :test #'string=)
    (set-difference *pt-faculty*
                    (mapcar #'cadr (ass-pt-matching)) :test #'string=)    
     *professor-map*)))
;;test
(length (rem-prof-rem-pt-matching))
(length *pt-faculty*)

;;;Step 4: Take second round of prof+as, match them to remaining part timers
(defun rem-prof-rem-ass-rem-pt-matching ()
  (sort-matching-by-last-name
   (match-professor-groups
    (union *prof-faculty* *ass-faculty*)
    (set-difference  *pt-faculty* (union
                                   (mapcar #'cadr (rem-prof-rem-pt-matching))
                                   (mapcar #'cadr (ass-pt-matching))))
    *professor-map*
    (concatenate 'list (ass-pt-matching) (rem-prof-rem-pt-matching)))))

(length  (rem-prof-rem-ass-rem-pt-matching))
;; we get 41 matchings
(concatenate 'list (prof-ass-matching)
                      (ass-pt-matching)
                      (rem-prof-rem-pt-matching)
                      (rem-prof-rem-ass-rem-pt-matching))

;; there should be 43 matchings
(+ (length *ass-faculty*) (length *pt-faculty*))

(set-difference (union *ass-faculty* *pt-faculty*)
                (mapcar #'cadr
                        (concatenate 'list (prof-ass-matching)
                      (ass-pt-matching)
                      (rem-prof-rem-pt-matching)
                      (rem-prof-rem-ass-rem-pt-matching))))

;;; => NIL
(set-difference (union *prof-faculty* *ass-faculty*)
                (mapcar #'car
                        (concatenate 'list (prof-ass-matching)
                      (ass-pt-matching)
                      (rem-prof-rem-pt-matching)
                      (rem-prof-rem-ass-rem-pt-matching))))
;;; => NIL
;;; All professors are used; all asssitants/part timers are watched
(sort-matching-by-last-name
 (concatenate 'list
              (prof-ass-matching)
              (ass-pt-matching)
              (rem-prof-rem-pt-matching)
              (rem-prof-rem-ass-rem-pt-matching)))

(+ (length *pt-faculty*) (length *ass-faculty*))


;; (defun print-matching ()
;;   (let* ((prof-ass-matching
;;           (sort-matching-by-last-name
;;            (match-professor-groups *prof-faculty* *ass-faculty* *professor-map*)))
         
;;          (prof-pt-matching
;;           (sort-matching-by-last-name
;;            (match-professor-groups 
;;             (union  (set-difference *prof-faculty* (mapcar #'car prof-ass-matching) :test #'string=) *ass-faculty*)
;;             *pt-faculty* *professor-map*)))
       
;;          (prof-rest-matching
;;           (sort-matching-by-last-name
;;            (match-professor-groups 
;;             (union *prof-faculty* *ass-faculty*)
;;             (set-difference *pt-faculty* (mapcar #'cadr prof-pt-matching) :test #'string=)  *professor-map*)))       
;;          (full-mapping (sort-matching-by-last-name (union prof-ass-matching (union prof-pt-matching prof-rest-matching)))))
  
;;     (format t "-- Professor To Assosciate------------- ~%")  
;;     (print-matching-pairs prof-ass-matching)
;;     ;; (format t "-- Unassigned Proffessor  + Ass to Part timers------------- ~%")  
;;     ;; (print-matching-pairs prof-pt-matching)
;;     ;; (format t "-- Prof+Ass to Remaining Part timers------------- ~%")  
;;     ;; (print-matching-pairs prof-rest-matching)
;;     (format t "-- Prof+Ass to All Part timers------------- ~%")  
;;     (print-matching-pairs (sort-matching-by-last-name (union prof-pt-matching prof-rest-matching)))
;;     (format t "--- Final Mapping ------ ~%")
;;     (print-matching-pairs full-mapping)
  
;;     (loop for m in  full-mapping
;;           for supervisor = (car m)
;;           for supervisee = (cadr m)
;;           do
;;           (format t "Supervisor -> Supervisee : [~a] -> [~a] ~%" supervisor supervisee)
;;           (format t "[~a] Courses:  ~%"  supervisor)
;;           (print-professor-courses supervisor *professor-map*)        
;;           (format t "[~a]Courses: ~%"  supervisee)
;;           (print-professor-courses supervisee *professor-map*))))

;; (print-matching)

;; ;;(professor-mapping *prof-faculty* *pt-faculty* *professor-map*)

;; (defun print-final-mapping ()
;;   (let* ((m-ft-pt (professor-mapping *prof-faculty* *pt-faculty* *professor-map*))
;;          (m-ft-ass (professor-mapping
;;                     (set-difference *prof-faculty* m-ft-pt 
;;                                     :test #'(lambda (a b) (equal (first b) a)))
;; 					*ass-faculty* *professor-map*))
;;          (m-res-ass (professor-mapping (union *prof-faculty* *pt-faculty*)
;;                                        (set-difference *ass-faculty* m-ft-ass :test #'(lambda (a b) (equal (first b) a)))
;; 					*professor-map*)))
;; 	(format t "-------------------------------------- ~%")
;; 	(format t "Full Time Mapping ~%")
;; 	(format t "---------------------------------------- ~%")
;; ;	(print-professor-groups *prof-faculty* *pt-faculty* *professor-map*)
;; 	(print-matching-pairs m-ft-pt)
;; 	(format t "---------------------------------------- ~%")
;; 	(format t "Part Time Mapping ~%")
;; 	(format t "---------------------------------------- ~%")
;; ;	(print-professor-groups *pt-faculty* *ass-faculty* *professor-map*))
;; 	(print-matching-pairs m-ft-ass)
;; 	(print-matching-pairs m-res-ass)))
;; ;;(assert (eql 8  (length *ass-faculty*)))
;; ;;(assert (eql 24  (length *prof-faculty*)))


;; (print-final-mapping)

;;; Tests
(let ((t1  (make-instance 'time-interval  :start 1 :end 10))
      (t2  (make-instance 'time-interval  :start 1 :end 10))
      (t3  (make-instance 'time-interval  :start 2 :end 8))
      (t4  (make-instance 'time-interval  :start 0 :end 2)))  
  (assert (time-interval-intersects t1 t2))
  (assert (time-interval-intersects t2 t1))
  (assert (time-interval-intersects t1 t3))
  (assert (time-interval-intersects t1 t4)))

