;;;***************
;;;* AUXILIAR FUNCTIONS *
;;;***************

;funcion para elegir la obra con mayor puntuacion que no se encuentre en nuestro recorrido actual
(deffunction find-max-not-selected (?obra $?already-selected)
  (bind ?max_punctuation (send ?obra get-puntuacion))
  (bind ?max_obra ?obra)
  (bind ?obras (find-all-instances ((?inst OBRA_DE_ARTE)) TRUE))
  (loop-for-count (?i 1 (length ?obras))
  (bind ?obra_actual (nth ?i ?obras))
  (bind ?puntuacion (send ?obra_actual get-puntuacion))
  (if (and (> ?puntuacion ?max_punctuation) (not (member ?obra_actual ?already-selected)) )
  then
    (bind ?max_punctuation ?puntuacion)
    (bind ?max_obra ?obra_actual)))
  ?max_obra
  )

;;;***************
;;;* RULES *
;;;***************

;regla para cambiar las obras del dia 1 de la solucion predeterminada por las obras con mayor puntuacion que son las que mas les va a interesar al visitante
(defrule refinement-visit ""
?oldvisit <- (visit $?list)
(not(visit2 $?))
=>
(bind $?listaux $?list)
(loop-for-count (?i 1 (length ?list))
(bind ?obra (nth ?i ?list))
(bind ?obra (find-max-not-selected ?obra $?listaux))
(bind ?listaux (replace$ ?listaux ?i ?i ?obra)))
(retract ?oldvisit)
(assert (visita-refinada ?listaux))
)

;regla para cambiar las obras del dia 1 y dia 2 de la solucion predeterminada por las obras con mayor puntuacion que son las que mas les va a interesar al visitante
(defrule refinement-visit2 ""
?oldvisit <- (visit $?list)
?oldvisit2 <- (visit2 $?list2)
(not (visit3 $?))
=>
(bind $?listaux $?list)
(bind $?listaux2 $?list2)
(bind $?listatotal $?list2)
(bind $?listatotal (insert$ $?listatotal 1 $?list))
(loop-for-count (?i 1 (length ?list))
(bind ?obra (nth ?i ?list))
(bind ?obra2 (nth ?i ?list2))
(bind ?obra (find-max-not-selected ?obra $?listatotal))
(bind ?listaux (replace$ ?listaux ?i ?i ?obra))
(bind ?listatotal (replace$ ?listatotal ?i ?i ?obra))
(bind ?obra2 (find-max-not-selected ?obra2 $?listatotal))
(bind ?listaux2 (replace$ ?listaux2 ?i ?i ?obra2))
(bind ?listatotal (replace$ ?listatotal (+ ?i (length ?list)) (+ ?i (length ?list)) ?obra2)))
(retract ?oldvisit)
(retract ?oldvisit2)
(assert (visita-refinada ?listaux))
(assert (visita-refinada2 ?listaux2))
)

;regla para cambiar las obras del dia 1, dia 2 y dia 3 de la solucion predeterminada por las obras con mayor puntuacion que son las que mas les va a interesar al visitante
(defrule refinement-visit3 ""
?oldvisit <- (visit $?list)
?oldvisit2 <- (visit2 $?list2)
?oldvisit3 <- (visit3 $?list3)
=>
(bind $?listaux $?list)
(bind $?listaux2 $?list2)
(bind $?listaux3 $?list3)
(bind $?listatotal $?list3)
(bind $?listatotal (insert$ $?listatotal 1 $?list2))
(bind $?listatotal (insert$ $?listatotal 1 $?list))
(loop-for-count (?i 1 (length ?list))
(bind ?obra (nth ?i ?list))
(bind ?obra2 (nth ?i ?list2))
(bind ?obra3 (nth ?i ?list3))
(bind ?obra (find-max-not-selected ?obra $?listatotal))
(bind ?listaux (replace$ ?listaux ?i ?i ?obra))
(bind ?listatotal (replace$ ?listatotal ?i ?i ?obra))
(bind ?obra2 (find-max-not-selected ?obra2 $?listatotal))
(bind ?listaux2 (replace$ ?listaux2 ?i ?i ?obra2))
(bind ?listatotal (replace$ ?listatotal (+ ?i (length ?list)) (+ ?i (length ?list)) ?obra2))
(bind ?obra3 (find-max-not-selected ?obra3 $?listatotal))
(bind ?listaux3 (replace$ ?listaux3 ?i ?i ?obra3))
(bind ?listatotal (replace$ ?listatotal (+ ?i (* (length ?list) 2)) (+ ?i (* (length ?list) 2)) ?obra3)))
(retract ?oldvisit)
(retract ?oldvisit2)
(retract ?oldvisit3)
(assert (visita-refinada ?listaux))
(assert (visita-refinada2 ?listaux2))
(assert (visita-refinada3 ?listaux3))
)

;regla para calcular la puntuacion de las obras
(defrule determine-puntuation ""
  (declare (salience 1))
  (painter-preferences $?pintores)
  (theme-preferences $?temas)
  (movement-preferences $?movimientos)
  (period-preferences $?periodos)
  (child-presence ?ninio)
  (cultural-level ?know)
  =>
  (bind ?obras (find-all-instances ((?inst OBRA_DE_ARTE)) TRUE))
  (loop-for-count (?i 1 (length ?obras))
  (bind ?obra (nth ?i ?obras))
  (bind ?puntuacion 0)
  (bind ?rele (send ?obra get-relevancia))
  (bind ?compl (send ?obra get-complejidad))
  (bind ?autor (send ?obra get-obra_autor))
  (bind ?tema (send ?obra get-obra_tematica))
  (bind ?movimiento (send ?obra get-obra_escuela))
  (bind ?periodo (send ?obra get-obra_epoca))
  (if (eq ?rele 1)
  then
    (bind ?puntuacion (+ ?puntuacion 10)))
  (if (eq ?rele 2)
  then
    (bind ?puntuacion (+ ?puntuacion 20)))
  (if (eq ?rele 3)
  then
    (bind ?puntuacion (+ ?puntuacion 30)))
  (if (eq (send ?obra get-contenido_adulto) TRUE)
  then
    (if (eq ?ninio TRUE)
    then
      (bind ?puntuacion (- ?puntuacion 1000))))
  (if (eq ?compl 1)
  then
    (if (eq ?know little)
    then
      (bind ?puntuacion (+ ?puntuacion 10))))
  (if (eq ?compl 2)
  then
    (if (eq ?know intermediate)
    then
      (bind ?puntuacion (+ ?puntuacion 10))))
  (if (eq ?compl 3)
  then
    (if (eq ?know expert)
    then
      (bind ?puntuacion (+ ?puntuacion 10))))
  (if (member ?autor ?pintores)
  then
    (bind ?puntuacion (+ ?puntuacion 100)))
  (if (member ?periodo ?periodos)
  then
    (bind ?puntuacion (+ ?puntuacion 100)))
  (loop-for-count (?j 1 (length ?temas))
  (bind ?tema_aux (nth ?j ?temas))
  (if (member ?tema_aux ?tema)
  then
    (bind ?puntuacion (+ ?puntuacion 100))))
  (loop-for-count (?j 1 (length ?movimientos))
  (bind ?movimiento_aux (nth ?j ?movimientos))
  (if (member ?movimiento_aux ?movimiento)
  then
    (bind ?puntuacion (+ ?puntuacion 100))))
    (send ?obra put-puntuacion ?puntuacion)))
