(in-package :com.bovinasancta.project)

(defparameter *pt-faculty* 
           '("s ahmed" "s arabhi" "m bodas" "s desousa" "c fan"
             "t fish" "t huynh" "k jensen" "a jiru"
              "j jordan" "o kovaleva" "r low" "j lum"
              "w newball" "l papay" "l sega" "t smith" "a strong" 
              "a talebi" "p tanniru" "j trubey" "j wang"
              "e zabric" "m zoubeidi" "s vergara"   "varitanian"
              "m van der poel"   "a tran"   "rober"   "rigers"
              "v nguyen" "t nguyen"   "a nguyen"   "hillard")
           "List of part time faculty")

(defparameter *ft-faculty*
          '("j becker" "m blockus" "r dodd" "l foster"
            "t hsu" "k kellum" "r kubelka" "h ng"
            "s obaid" "b pence" "b peterson" "f rivera"
            "m saleem" "e schmeichel" "w so" "m stanley" 
            "c roddick" "sliva-spitzer" "shubin"  "pfifer"
            "katsura" "jackson" "beason" "alperin")
          "List of full time faculty")

(defparameter *ass-faculty*
           '( "s crunk" "a gottlieb" "p koev" "b lee"
              "j maruskin" "s simic"  "m cayco-gajic"  "bremer" )
           "List of Associate or Assistant Professors")


(defclass time-interval ()
  ((start  :initarg :start :initform 0 :accessor interval-start)
   (end  :initarg :end :initform 0  :accessor interval-end)))

(defun string->time-interval (str)
  (if (and  str (> (length str) 0) (not (string=  "tba" str)) )
      (let* ((str-pair (split-by-char str #\-))
             (start   (parse-integer (first str-pair)))
             (end     (parse-integer (second str-pair))))    
        (make-instance 'time-interval :start start :end end))))

(defclass name()
  ((first-name :initarg :first :accessor first-name)
   (last-name  :initarg :last  :accessor last-name)))

(defun string->name (name)
  (let* ((names (str/split-by-one-space name))
         (first-name "")
         (last-name (first names)))
    (if (> (length names) 1)
        (progn
          (setq first-name (first names))
          (setq last-name (second names)))) 
    (make-instance 'name :first first-name :last last-name)))

(defclass course-data ()
  ((name :initarg :course-name 
         :accessor course-name)
   (title :initarg :course-title
          :accessor course-title)
   (section :initarg :course-section
            :accessor course-section)
   (code :initarg :course-code
         :accessor course-code)
   (credit :initarg :course-credit
         :accessor course-credit)
   (type :initarg :course-type
         :accessor course-type)
   (time :accessor course-time)   
   (notes :initarg :course-notes
          :accessor course-notes)
   (capacity :initarg :course-capacity
             :accessor course-capacity)
   (dates :initarg :course-dates
          :accessor course-dates)
   (days :initarg :course-days
          :accessor course-days)
   (location :initarg :course-location
             :accessor course-location)
   (professor :accessor course-professor)))

(defmethod initialize-instance :before ((obj course-data)  &key course-professor course-time   &allow-other-keys)
  (if nil
      (format t "Before :Creating course-data name [~a] time[~a] ~%" course-professor course-time ))
  (setf (slot-value obj 'professor) (string->name course-professor))
  (setf (slot-value obj 'time) (string->time-interval course-time)))

(defun professor-map->names(map)
  (hash-keys *professor-map*))

(defun professor-coures-map (courses)
  (loop for course in courses
        for prof = (course-professor course)
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

(defun read-course-data(file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (parsed-data '()))
    (when in
      (loop for term =  (read in nil)
         for course-data = (pre-process-term term)
         while term
         do      
           (push (apply #'make-instance 'course-data course-data) parsed-data))      
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

(defun professor-overlapping-coursesp (prof1 prof2 map)
  (let ((prof1-courses  (professor-find-courses prof1 map))
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

(defun professor-supervisable (prof1 prof2 map)
  (not (professor-overlapping-coursesp prof1 prof2 map)))

(defun professor-mapping (ls1 ls2 map)
  (loop for prof1 in ls1
        with mapping = '()
        finally (return mapping)
        do
        (loop for prof2 in ls2
              do
              (if (professor-supervisable prof1 prof2 map )
                  (push (list prof1 prof2) mapping)))))


(defun day-time-overlap(day1 time1 day2 time2 )
  (and  (days-intersect day1 day2) 
        (time-interval-intersects time1 time2)))

(defun print-matching-pairs (matching)
  (loop for pair in matching
        do
       (format t "~a -> ~a ~%" (first  pair) (second  pair))))

(defun print-basic-course-data (course)
  (format t "~%")
  (format t "Course Title[~a]~%Course Professor[~a]~%Course Days [~a]~%Course Time[~a-~a]~%" 
          (course-title course)
          (course-professor course)
          (course-days course)
          (interval-start (course-time course))
          (interval-end (course-time course)))  
  (format t "~%"))

(defun print-professor-groups(group1 group2 map)
  (let* ((avialable-matches
          (professor-mapping group1 group2  map))
         (matches (maximal-matching avialable-matches)))
    (print-matching-pairs matches)))

(defparameter *courses* (read-course-data "data.lisp"))
(defparameter *professor-map* (professor-coures-map *courses*))

(defun print-final-mapping ()
  (format t "-------------------------------------- ~%")
  (format t "Full Time Mapping ~%")
  (format t "---------------------------------------- ~%")
  (print-professor-groups *ft-faculty* *pt-faculty* *professor-map*)
  (format t "---------------------------------------- ~%")
  (format t "Part Time Mapping ~%")
  (format t "---------------------------------------- ~%")
  (print-professor-groups *pt-faculty* *ass-faculty* *professor-map*))

(print-final-mapping)


;;; Tests
(let ((t1  (make-instance 'time-interval  :start 1 :end 10))
      (t2  (make-instance 'time-interval  :start 1 :end 10))
      (t3  (make-instance 'time-interval  :start 2 :end 8))
      (t4  (make-instance 'time-interval  :start 0 :end 2)))  
  (assert (time-interval-intersects t1 t2))
  (assert (time-interval-intersects t2 t1))
  (assert (time-interval-intersects t1 t3))
  (assert (time-interval-intersects t1 t4)))

