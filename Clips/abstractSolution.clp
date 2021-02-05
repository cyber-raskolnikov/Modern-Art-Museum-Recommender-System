;;;***************
;;;* ABSTRACT SOLUTION RULES *
;;;***************

;regla que nos asigna la solucion predeterminada si vamos a ir un dia durante una hora
(defrule default-mapping-short1 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration one)
  (hours-duration short)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "short1"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (assert (visit ?dayone)))

;regla que nos asigna la solucion predeterminada si vamos a ir dos dias durante una hora
(defrule default-mapping-short2 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration two)
  (hours-duration short)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "short2"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (bind ?daytwo (send ?recorrido get-dayTwo))
  (assert (visit ?dayone))
  (assert (visit2 ?daytwo)))

;regla que nos asigna la solucion predeterminada si vamos a ir tres dias durante una hora
(defrule default-mapping-short3 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration three)
  (hours-duration short)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "short3"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (bind ?daytwo (send ?recorrido get-dayTwo))
  (bind ?daythree (send ?recorrido get-dayThree))
  (assert (visit ?dayone))
  (assert (visit2 ?daytwo))
  (assert (visit3 ?daythree)))

;regla que nos asigna la solucion predeterminada si vamos a ir un dia durante dos/tres horas
(defrule default-mapping-intermediate1 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration one)
  (hours-duration intermediate)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "intermediate1"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (assert (visit ?dayone)))

;regla que nos asigna la solucion predeterminada si vamos a ir dos dias durante dos/tres horas
(defrule default-mapping-intermediate2 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration two)
  (hours-duration intermediate)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "intermediate2"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (bind ?daytwo (send ?recorrido get-dayTwo))
  (assert (visit ?dayone))
  (assert (visit2 ?daytwo)))

;regla que nos asigna la solucion predeterminada si vamos a ir tres dias durante dos/tres horas
(defrule default-mapping-intermediate3 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration three)
  (hours-duration intermediate)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "intermediate3"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (bind ?daytwo (send ?recorrido get-dayTwo))
  (bind ?daythree (send ?recorrido get-dayThree))
  (assert (visit ?dayone))
  (assert (visit2 ?daytwo))
  (assert (visit3 ?daythree)))

;regla que nos asigna la solucion predeterminada si vamos a ir un dia durante cuatro horas
(defrule default-mapping-long1 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration one)
  (hours-duration long)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "long1"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (assert (visit ?dayone)))

;regla que nos asigna la solucion predeterminada si vamos a ir dos dias durante cuatro horas
(defrule default-mapping-long2 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration two)
  (hours-duration long)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "long2"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (bind ?daytwo (send ?recorrido get-dayTwo))
  (assert (visit ?dayone))
  (assert (visit2 ?daytwo)))

;regla que nos asigna la solucion predeterminada si vamos a ir tres dias durante cuatro horas
(defrule default-mapping-long3 ""
  (declare (salience 0))
  (period-preferences $?)
  (days-duration three)
  (hours-duration long)
  ?recorrido <- (object (is-a RECORRIDO) (pred_name ?name))
  (test (eq ?name "long3"))
  =>
  (bind ?dayone (send ?recorrido get-dayOne))
  (bind ?daytwo (send ?recorrido get-dayTwo))
  (bind ?daythree (send ?recorrido get-dayThree))
  (assert (visit ?dayone))
  (assert (visit2 ?daytwo))
  (assert (visit3 ?daythree)))
