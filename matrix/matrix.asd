(defsystem "matrix"
	 :version "0.1.0"
	 :description "我的矩阵库"
	 :components
	 ((:file "generic")
	  (:file "matrix-template" :depends-on ("generic"))))
