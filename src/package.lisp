(defpackage competitive
  (:use #:cl #:alexandria #:serapeum)
  (:local-nicknames 
   (:threads :bordeaux-threads)
   (:json :com.inuoe.jzon)
   (:regex :cl-ppcre)))