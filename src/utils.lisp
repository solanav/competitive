(in-package #:competitive)

;; alias

(defalias to-list (rcurry #'coerce 'list))
(defalias ->l (rcurry #'coerce 'list))
(defalias remove-nulls (curry #'remove-if #'null))
(defalias out (curry #'format t))

;; macros

(defmacro >> (&rest functions)
  "shorthand for composing functions"
  `(compose ,@(loop :for f :in functions :append `(#',f))))

(defmacro ~ (function &rest args)
  "shorthand for currying functions"
  `(curry #',function ,@args))

(defmacro ~~ (function &rest args)
  "shorthand for currying functions"
  `(rcurry #',function ,@args))

(defmacro returning (value &body body)
  `(progn ,@body ,value))

(defmacro unpacking (list &body body)
  `(let ((car (car ,list))
         (cadr (cadr ,list))
         (cdr (cdr ,list)))
     (declare (ignorable car cadr cdr))
     ,@body))

;; functions

(defun addhash (key value hash-table)
  "Lookup KEY in HASH-TABLE, add VALUE inside a list if KEY does not exist, cons it otherwise"
  (multiple-value-bind (old-value old-value-exists)
      (gethash key hash-table)
    (setf (gethash key hash-table)
          (if old-value-exists 
              (if (listp old-value)
                  (cons value old-value)
                  (cons value (list old-value)))
              (list value)))))

(defun remove-nth (sequence index)
  "Remove the nth element of a sequence"
  (splice-seq sequence :start index :end (min (length sequence) (1+ index))))

(defun random-index (sequence)
  "Returns a random index of a sequence"
  (random (length sequence)))

(defun random-pop (sequence)
  "Pop a random value of the sequence and return it"
  (remove-nth sequence (random-index sequence))) 

(defun average (sequence)
  (/ (reduce #'+ sequence) (length sequence)))

(defun sh (control-string &rest format-arguments)
  "Execute a command in the local machine"
  (slice 
   (with-output-to-string (s)
     (uiop:run-program 
      (apply #'fmt (cons control-string format-arguments))
      :output s :error-output s :ignore-error-status t)
     s) 0 -1))

(defun worst (score sequence)
  "return the index of the worst scoring elements in the sequence"
  (let ((worst (reduce #'min (mapcar score (enumerate sequence)))))
    (loop :for e :in sequence
          :for i := 0 :then (1+ i)
          :if (= (funcall score i) worst)
          :collect i)))

(defun last-digit (number)
  (cadr (multiple-value-list (floor number 10))))

(defun enumerate (sequence)
  "return indexes in sequence"
  (loop :for i :upto (1- (length sequence))
        :collect i))

(defun minimize (key sequence)
  (let ((min (apply #'min (mapcar key sequence))))
    (loop :for e :in sequence
          :if (= (funcall key e) min)
          :collect e)))

(defun maximize (key sequence)
  (let ((max (apply #'max (mapcar key sequence))))
    (loop :for e :in sequence
          :if (= (funcall key e) max)
          :collect e)))

(defun best (score sequence)
  "return the index of the best scoring elements in the sequence"
  (loop :with best := (apply #'max (mapcar score sequence))
        :for e :in sequence
        :for i := 0 :then (1+ i)
        :if (= (funcall score e) best)
        :collect i))

(defun swap-nth (sequence index new)
  (splice-seq sequence :new (list new) :start index :end (1+ index)))

(defun half (number)
  (floor number 2))

(defun list->integer (l)
  (loop :for n :in (reverse l)
        :for i := 1 :then (* 10 i)
        :sum (* n i)))

(defun >< (number min max)
  "checks that number is between min and max, both included"
  (and (<= number max) (>= number min)))

(defun <> (number min max)
  "checks that number is not between min and max, both included"
  (not (>< number min max)))

(defun char->digit (c)
  (declare (type character c))
  (- (char-code c) 48))

(defun flatten-once (tree)
  (mapcan #'identity tree))

(defun read-file (file)
  (read-file-into-string
   (asdf:system-relative-pathname
    (make-keyword (package-name *package*)) file)))

;; AoC specific stuff

(defun aoc-input (day)
  (~> (fmt "https://adventofcode.com/2025/day/~a/input" day)
      (dex:get :headers `(("Cookie" . ,(read-file ".cookie"))))
      (lines)))
