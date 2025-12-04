(defsystem "competitive"
  :version "1.0.0"
  :author "Antonio Solana"
  :license "AGPL"
  :depends-on ("alexandria"
               "serapeum"
               "bordeaux-threads"
               "com.inuoe.jzon"
               "dexador"
               "cl-ppcre")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "main"))))
  :description "Utils for more comfortable problem solving")
