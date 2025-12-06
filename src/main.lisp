(in-package #:competitive)

(defun cses-1068 (n)
  (if (= n 1)
      '(1) 
      (cons n (cses-1068
               (if (evenp n)
                   (/ n 2)
                   (1+ (* n 3)))))))

(defun cses-1083 (max sequence)
  (declare (ignore max))
  (loop :for old-n := 0 :then n
        :for n :in (->l (sort sequence #'<))
        :if (not (= n (1+ old-n)))
        :do (return (1- n))))

(defun cses-1069 (sequence)
  (loop :with longest := 0
        :for i := 0 :then (1+ i)
        :for old-char := #\Z :then char
        :for char :in (->l sequence)
        :if (not (char= old-char char))
        :do (setf longest (max i longest) i 0)
        :finally (return longest)))

(defun aoc202501-rotate (number step times)
  (loop :with zeros := 0
        
        :for first-iter := t :then nil
        :for new-number := number :then (mod (+ new-number step) 100)
        :for new-times := times :then (1- new-times)
        
        :if (zerop new-number) :do (when (not first-iter) (incf zeros))
        
        :while (plusp new-times)
        
        :finally (return (cons new-number zeros))))

(defun aoc202501 (input &optional count-pass)
  (loop :with zeros := 0
        :with number := 50
        
        :for instruction :in input
        
        :for step := (let ((c (char instruction 0))) (if (char= c #\L) -1 1))
        :for times := (parse-integer (subseq instruction 1))
        :for (new-number . new-zeros) := (aoc202501-rotate number step times)
        
        :do (setf number new-number)
        
        :if count-pass :do (incf zeros new-zeros)
        :if (and (not count-pass) (zerop number)) :do (incf zeros)
        
        :finally (return zeros)))

(defun aoc202502-parse (input)
  (flet ((list->integer (l) 
           (parse-integer (coerce (reverse l) 'string))))
    (loop :with ranges := nil 
          :with x := nil ;; first number
          :with y := nil ;; second number
          :with buff := nil ;; buffer to accumulate
        
          :for c :across input 
        
          :do (cond 
                ((char= c #\-)
                 (setf x (list->integer buff) buff nil))
                ((char= c #\,) 
                 (setf y (list->integer buff) buff nil)
                 (setf ranges (cons (cons x y) ranges)))
                (t (setf buff (cons c buff))))
        
          :finally (progn
                     ;; parse remaining data
                     (setf y (list->integer buff) buff nil)
                     (setf ranges (cons (cons x y) ranges))
                   
                     ;; return the ranges
                     (return (reverse ranges))))))

(defun aoc202502-repeated? (number)
  (let* ((number-string (fmt "~a" number))
         (number-len (length number-string)))
    (if (evenp number-len)
        (string= (subseq number-string 0 (half number-len))
                 (subseq number-string (half number-len)))
        nil)))

(defun aoc202502-seq-lens (number)
  (let* ((number-string (fmt "~a" number))
         (number-len (length number-string))
         (max-seq-len (half number-len))
         (end (if (evenp number-len)
                  (1+ max-seq-len)
                  max-seq-len)))
    (if (= end 1) (vector 1) (range 1 end))))

(defun aoc202502-seq-repeated? (number seq-len)
  (let* ((number-string (fmt "~a" number))
         (number-len (length number-string))
         (subseqs (loop :for last-i := 0 :then i
                        :for i := seq-len :then (+ i seq-len)
                        :until (> i number-len)
                        :collect (subseq number-string last-i i)))
         (first (elt subseqs 0)))
    (when (zerop (mod number-len seq-len))
      (every (~ string= first) subseqs))))

(defun aoc202502-repeated2? (number)
  (when (> number 9)
    (some (~ aoc202502-seq-repeated? number) 
          (aoc202502-seq-lens number))))

(defun aoc202502 (input)
  (loop :for (start . end) :in (aoc202502-parse input)
        :sum (sum (remove-if-not
                   #'aoc202502-repeated2?
                   (to-list (range start (1+ end)))))))

(defun aoc202503-max (input &key exclude)
  (values-list
   (loop :with max := (list -1 0)
         :for n :in input
         :for i := 0 :then (1+ i)
         :if (and (not (member i exclude)) (> n (cadr max)))
         :do (setf max (list i n))
         :finally (return max))))

(defun aoc202503-line (line &key (need 12) (exclude nil) (offset 0))
  (when (plusp need) 
    (multiple-value-bind (index number) (aoc202503-max line :exclude exclude)
      (let ((need (1- need)) 
            (len-right (- (length line) index 1))
            (seq-right (subseq line (1+ index))))
        (cond ((= len-right need)
               (cons number seq-right))
              ((> len-right need)
               (cons number (aoc202503-line 
                             seq-right
                             :need need 
                             :exclude nil 
                             :offset index)))
              (t (aoc202503-line 
                  line
                  :need (1+ need)
                  :exclude (cons index exclude)
                  :offset offset)))))))

(defun aoc202503 ()
  (assert 
   (= (loop :for raw-line :in (aoc-input 3) 
            :for line := (map 'list #'char->digit raw-line)
            :sum (list->integer (aoc202503-line line)))
      171975854269367)))

(defun aoc202504-parse (input)
  (loop :for line :in input 
        :collect (map 'list #'aoc202504-read-point
                      (trim-whitespace line))))

(defun aoc202504-read-point (value)
  (switch (value :test #'char=)
    (#\. 0) (#\@ 1) (#\x 2)))

(defun aoc202504-draw-point (value)
  (switch (value :test #'=)
    (0 ".") (1 "@") (2 "x")))

(defun aoc202504-draw-map (map)
  (out "  ")
  (loop :for i :in (enumerate (car map))
        :do (out "~a" i))
  (out "~%")
  (loop :for line :in map
        :for i := 0 :then (1+ i)
        :do (out "~a " i)
        :do (loop :for c :in line
                  :do (out "~a" (aoc202504-draw-point c)))
        :do (out "~%")))

(defun aoc202504-1 ()
  (labels ((xy (map x y) 
             (cond 
               ((<> x 0 (1- (length (nth 0 map)))) 0)
               ((<> y 0 (1- (length map))) 0)
               (t (nth x (nth y map)))))
           (paper (map x y)
             (= 1 (xy map x y)))
           (nb (map x y)
             (list 
              (list (xy map (1- x) (1- y)) (xy map x (1- y)) (xy map (1+ x) (1- y)))
              (list (xy map (1- x) y)      (xy map x y)      (xy map (1+ x) y))
              (list (xy map (1- x) (1+ y)) (xy map x (1+ y)) (xy map (1+ x) (1+ y)))))
           (nbc (map x y)
             (- (loop :for l :in (nb map x y)
                      :sum (reduce #'+ l))
                (xy map x y)))
           (mapmap (function map)
             (loop :for y :in (enumerate map)
                   :collect (loop :for x :in (enumerate (car map))
                                  :collect (funcall function x y)))))
    (let ((map (aoc202504-parse (aoc-input 4))))
      (reduce #'+
              (flatten
               (mapmap (lambda (x y) 
                         (if (and (paper map x y) (< (nbc map x y) 4))
                             1 0))
                       map))))))
(defun aoc202504-2 ()
  (labels ((xy (map x y) 
             (cond 
               ((<> x 0 (1- (length (nth 0 map)))) 0)
               ((<> y 0 (1- (length map))) 0)
               (t (nth x (nth y map)))))
           (paper? (map x y)
             (= 1 (xy map x y)))
           (nb (map x y)
             (list 
              (list (xy map (1- x) (1- y)) (xy map x (1- y)) (xy map (1+ x) (1- y)))
              (list (xy map (1- x) y)      (xy map x y)      (xy map (1+ x) y))
              (list (xy map (1- x) (1+ y)) (xy map x (1+ y)) (xy map (1+ x) (1+ y)))))
           (nbc (map x y)
             (- (loop :for l :in (nb map x y)
                      :sum (reduce #'+ l))
                (xy map x y)))
           (mapmap (function map)
             (loop :for y :in (enumerate map)
                   :collect (loop :for x :in (enumerate (car map))
                                  :collect (funcall function x y))))
           (removable (map)
             (mapmap (lambda (x y) 
                       (list (and (paper? map x y) (< (nbc map x y) 4))
                             x y))
                     map))
           (paper (map)
             (reduce #'+ (flatten map)))
           (rp (map x y) 
             (let ((row (nth y map))) (setf (nth x row) 0)))
           (clean (map) 
             (~>> (removable map)
                  (flatten-once)
                  (remove-if-not #'identity _ :key #'car)
                  (mapcar #'cdr)
                  (mapcar (lambda (p) (rp map (car p) (cadr p)))))))
    (let* ((map (aoc202504-parse (aoc-input 4)))
           (total-paper (paper map)))
      (loop :for last-paper := 0 :then paper
            :for paper := (paper map)
            :until (= paper last-paper)
            :do (progn 
                  ;; (out "Paper found: ~a~%" (paper map))
                  ;; (aoc202504-draw-map map)
                  (clean map)))
      (- total-paper (paper map)))))

(defun aoc202505-parse-range (range)
  (list-bind (min max) 
      (mapcar #'parse-integer (split-sequence #\- range))
    (~~ >< min max)))

(defun aoc202505-parse (input)
  (loop :with range := t
        :with lambdas :with numbers
        :for line :in (mapcar #'trim-whitespace input)
        :do (cond ((string= line "") (setf range nil))
                  (range (consf (aoc202505-parse-range line) lambdas))
                  (t (consf (parse-integer line) numbers)))
        :finally (return (list lambdas numbers))))

(defun aoc202505-1 (input)
  (list-bind (lambdas numbers) (aoc202505-parse input)
    (sum (mapcar (op (bool->int (at-least-one (mapcar (~~ funcall _) lambdas)))) numbers))))

(defun aoc202505-2 (input)
  (labels ((parse-range (l) (mapcar #'parse-integer (split-sequence #\- l)))
           (parse-input (i) (loop :for raw-line :in i
                                  :for line := (trim-whitespace raw-line)
                                  :until (string= line "")
                                  :collect (parse-range line)))
           (merge-ranges (r0 r1) 
             (if r1
                 (list-bind (a b) r0
                   (list-bind (c d) r1
                     (cond ((and (>= b c) (<= b d)) (list a d))
                           ((and (>= b c) (>= b d)) r0)
                           ((and (= a c) (= b d)) r0)
                           (t nil))))))
           (merge-once (ranges) 
             (remove-nulls
              (loop :with skip
                    :for (r0 r1) :on ranges
                    :for r2 := (merge-ranges r0 r1)
                    :collect (cond (skip (setf skip nil))
                                   (r2 (setf skip r2))
                                   (t r0)))))
           (clean-ranges (r0 r1)
             (if r1 
                 (list-bind (a b) r0
                   (list-bind (c d) r1
                     (cond ((and (>= c a) (<= d b)) r0)
                           ((and (>= a c) (<= b d)) r1)
                           (t nil))))
                 nil))
           (clean-once (ranges)
             (remove-nulls
              (loop :with skip
                    :for (r0 r1) :on ranges
                    :for r2 := (clean-ranges r0 r1)
                    :collect (cond (skip (setf skip nil))
                                   (r2 (setf skip r2))
                                   (t r0)))))
           (clean-merge (ranges)
             (merge-once 
              (run-until-settled (ranges :test #'equal)
                (clean-once .last-result.)))))
    
    (loop :for (a b) :in (clean-merge (sort (parse-input input) #'< :key #'car)) 
          :sum (- (1+ b) a))

    (loop :for (a b) :in (run-until-settled 
                             ((sort (parse-input input) #'< :key #'car) :test #'equal)
                           (merge-once .last-result.))
          :sum (- (1+ b) a)))))

(defparameter *test*
(lines "3-5
        10-14
        16-20
        12-18

        1
        5
        8
        11
        17
        32"))