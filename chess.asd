(defsystem "chess"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria"
               "fset"
               "arrows"
               "cl-ppcre"
               "str")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "constants")
                 (:file "board"))))
  :description ""
  :in-order-to ((test-op (test-op "chess/tests"))))

(defsystem "chess/tests"
  :author ""
  :license ""
  :depends-on ("chess"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for chess"
  :perform (test-op (op c) (symbol-call :rove :run c)))
