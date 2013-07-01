(in-package "TSP")

(defclass ranking-process (coordinator-process) ())
; weiß noch nicht, ob das wirklich ein coordinator-process ist oder was anderes

(defmethod process-run ((proc ranking-process))
; wenn genügend candidate-points auf BB sind, ein Ranking daraus erstellen
)
