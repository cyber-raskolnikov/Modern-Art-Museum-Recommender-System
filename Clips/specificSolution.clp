;;****************
;;* FUNCTIONS *
;;****************

;funcion que nos devuelve el tiempo que va a estar el visitante viendo una obra dependiendo de su complejidad, del tamaño del grupo y del nivel de arte del grupo
;los tiempos en si se entra mas en detalle en la documentacion
(deffunction get-art-time (?complexity ?size ?knowledge $?tl)
  (bind ?complexity (- ?complexity 1))
  (if (eq ?size single)
    then (bind ?s 0))
  (if (eq ?size couple)
    then (bind ?s 1))
  (if (eq ?size small)
    then (bind ?s 2))
  (if (eq ?size big)
    then (bind ?s 3))
  (if (eq ?knowledge little)
    then (bind ?k 0))
  (if (eq ?knowledge intermediate)
    then (bind ?k 1))
  (if (eq ?knowledge expert)
    then (bind ?k 2))
  (bind ?index (+ (+ (+ 1 ?s) (* 4 ?k)) (* 12 ?complexity)))
  (bind ?time (nth ?index ?tl))

  ?time
  )

;funcion que devuelve el indice de la obra que esta asignada a nuestro recorrido con menor puntuacion, se utiliza para eliminar las obras que menos nos interesan cuando excedemos el tiempo de la visita
(deffunction get-min-index ($?obras) ""
(bind ?min_obra (nth 1 ?obras))
(bind ?min_index 1)
(bind ?min_punctuation (send ?min_obra get-puntuacion))
(loop-for-count (?i 1 (length ?obras))
(bind ?obra_actual (nth ?i ?obras))
(bind ?puntuacion (send ?obra_actual get-puntuacion))
(if (< ?puntuacion ?min_punctuation)
then
(bind ?min_punctuation ?puntuacion)
(bind ?min_index ?i)))
?min_index)


;;;****************************
;;;* VISIT TIME RULES *
;;;****************************

;regla que recorta el recorrido del dia 1 de la visita, solo si excedemos la duracion
(defrule times-visit ""
(declare (salience -4))
?oldvisit <- (visita-refinada $?list)
(group-size ?size)
(cultural-level ?knowledge)
(child-presence ?child)
(time-list $?tl)
(hours-duration ?visit-time)
(not (tiempo-visita ?))
=>
(bind $?listaux $?list)
(bind ?tiempo-total 0)
(if (eq ?visit-time short)
then
(bind ?max-time 60))
(if (eq ?visit-time intermediate)
then
(bind ?max-time 180))
(if (eq ?visit-time long)
then
(bind ?max-time 240))
(bind $?visit-times (create$ ))
(bind ?counter 0)
(bind ?gone_too_far FALSE)
(loop-for-count (?i 1 (length ?list))
(bind ?element (nth ?i ?list))
(bind ?complexity (send ?element get-complejidad))
(bind ?art-time 0)
(if (eq ?child TRUE)
then
(bind ?art-time 5)
else
(bind ?art-time (get-art-time ?complexity ?size ?knowledge $?tl)))
(bind ?visit-times (insert$ $?visit-times ?i ?art-time))
(if (< ?tiempo-total ?max-time)
then
(bind ?tiempo-total (+ ?art-time ?tiempo-total))
else
(bind ?gone_too_far TRUE)
(bind ?counter (+ ?counter 1))))
(if (eq ?gone_too_far TRUE)
then
(assert (was_too_long TRUE))
(loop-for-count (?i 1 ?counter)
(bind ?index (get-min-index $?listaux))
(bind ?listaux (delete$ $?listaux ?index ?index))
(bind ?visit-times (delete$ $?visit-times ?index ?index))))
;(bind ?listaux (delete$ $?listaux (length ?listaux) (length ?listaux)))
(retract ?oldvisit)
(assert (total-time-visit ?tiempo-total))
(assert (visita-recortada ?listaux))
(assert (tiempo-visita ?visit-times)))

;regla que recorta el recorrido del dia 2 de la visita, solo si excedemos la duracion
(defrule times-visit2 ""
(declare (salience -4))
?oldvisit <- (visita-refinada2 $?list)
(group-size ?size)
(cultural-level ?knowledge)
(child-presence ?child)
(time-list $?tl)
(hours-duration ?visit-time)
(not (tiempo-visita2 ?))
=>
(bind $?listaux $?list)
(bind ?tiempo-total 0)
(if (eq ?visit-time short)
then
(bind ?max-time 60))
(if (eq ?visit-time intermediate)
then
(bind ?max-time 180))
(if (eq ?visit-time long)
then
(bind ?max-time 240))
(bind $?visit-times (create$ ))
(bind ?counter 0)
(bind ?gone_too_far FALSE)
(loop-for-count (?i 1 (length ?list))
(bind ?element (nth ?i ?list))
(bind ?complexity (send ?element get-complejidad))
(bind ?art-time 0)
(if (eq ?child TRUE)
then
(bind ?art-time 5)
else
(bind ?art-time (get-art-time ?complexity ?size ?knowledge $?tl)))
(bind ?visit-times (insert$ $?visit-times ?i ?art-time))
(if (< ?tiempo-total ?max-time)
then
(bind ?tiempo-total (+ ?art-time ?tiempo-total))
else
(bind ?gone_too_far TRUE)
(bind ?counter (+ ?counter 1))))
(if (eq ?gone_too_far TRUE)
then
(assert (was_too_long TRUE))
(loop-for-count (?i 1 ?counter)
(bind ?index (get-min-index $?listaux))
(bind ?listaux (delete$ $?listaux ?index ?index))
(bind ?visit-times (delete$ $?visit-times ?index ?index))))
;(bind ?listaux (delete$ $?listaux (length ?listaux) (length ?listaux)))
(retract ?oldvisit)
(assert (total-time-visit2 ?tiempo-total))
(assert (visita-recortada2 ?listaux))
(assert (tiempo-visita2 ?visit-times)))

;regla que recorta el recorrido del dia 3 de la visita, solo si excedemos la duracion
(defrule times-visit3 ""
(declare (salience -4))
?oldvisit <- (visita-refinada3 $?list)
(group-size ?size)
(cultural-level ?knowledge)
(child-presence ?child)
(time-list $?tl)
(hours-duration ?visit-time)
(not (tiempo-visita3 ?))
=>
(bind $?listaux $?list)
(bind ?tiempo-total 0)
(if (eq ?visit-time short)
then
(bind ?max-time 60))
(if (eq ?visit-time intermediate)
then
(bind ?max-time 180))
(if (eq ?visit-time long)
then
(bind ?max-time 240))
(bind $?visit-times (create$ ))
(bind ?counter 0)
(bind ?gone_too_far FALSE)
(loop-for-count (?i 1 (length ?list))
(bind ?element (nth ?i ?list))
(bind ?complexity (send ?element get-complejidad))
(bind ?art-time 0)
(if (eq ?child TRUE)
then
(bind ?art-time 5)
else
(bind ?art-time (get-art-time ?complexity ?size ?knowledge $?tl)))
(bind ?visit-times (insert$ $?visit-times ?i ?art-time))
(if (< ?tiempo-total ?max-time)
then
(bind ?tiempo-total (+ ?art-time ?tiempo-total))
else
(bind ?gone_too_far TRUE)
(bind ?counter (+ ?counter 1))))
(if (eq ?gone_too_far TRUE)
then
(assert (was_too_long TRUE))
(loop-for-count (?i 1 ?counter)
(bind ?index (get-min-index $?listaux))
(bind ?listaux (delete$ $?listaux ?index ?index))
(bind ?visit-times (delete$ $?visit-times ?index ?index))))
;(bind ?listaux (delete$ $?listaux (length ?listaux) (length ?listaux)))
(retract ?oldvisit)
(assert (total-time-visit3 ?tiempo-total))
(assert (visita-recortada3 ?listaux))
(assert (tiempo-visita3 ?visit-times)))

;;;****************************
;;;* VISIT PRINTING RULES *
;;;****************************

;regla que mostrara por pantalla la visita sugerida para el dia 1
(defrule print-visit ""
(declare (salience -5))
(visita-recortada $?list)
(tiempo-visita $?tiempos)
=>
(bind ?counter 0)
(loop-for-count (?k 1 (length ?tiempos))
(bind ?arttime (nth ?k ?tiempos))
(bind ?counter (+ ?counter ?arttime)))
(printout t crlf crlf)
(printout t "Suggested Visit")
(printout t crlf crlf)
(printout t "--------------")
(printout t "-- Day One: --")
(printout t "--------------")
(printout t "     VISIT TIME: " ?counter)
(printout t crlf crlf)
(bind ?salas (find-all-instances ((?inst SALA)) TRUE))
(loop-for-count (?i 1 (length ?salas))
(bind ?sala (nth ?i ?salas))
(bind ?obras-sala (send ?sala get-sala_obra))
(bind ?foundany FALSE)
(loop-for-count (?j 1 (length ?list))
(bind ?element (nth ?j ?list))
(if (member ?element ?obras-sala)
then
(if (eq ?foundany FALSE)
then
(bind ?foundany TRUE)
(printout t "SALA " (send (send ?sala get-sala_escuela) get-nombre_escuela) ":" crlf)
(printout t crlf))
(printout t "   "(send ?element get-nombre_obra) " ")
(printout t "[VISIT TIME: " (nth ?j ?tiempos) " MINUTES]" crlf)))
(if (eq ?foundany TRUE)
then
(printout t crlf))
))

;regla que mostrara por pantalla la visita sugerida para el dia 2
(defrule print-visit2 ""
(declare (salience -6))
(visita-recortada2 $?list)
(tiempo-visita2 $?tiempos)
=>
(bind ?counter 0)
(loop-for-count (?k 1 (length ?tiempos))
(bind ?arttime (nth ?k ?tiempos))
(bind ?counter (+ ?counter ?arttime)))
(printout t  crlf)
(printout t "--------------")
(printout t "-- Day Two: --")
(printout t "--------------")
(printout t "     VISIT TIME: " ?counter)
(printout t crlf crlf)
(bind ?salas (find-all-instances ((?inst SALA)) TRUE))
(loop-for-count (?i 1 (length ?salas))
(bind ?sala (nth ?i ?salas))
(bind ?obras-sala (send ?sala get-sala_obra))
(bind ?foundany FALSE)
(loop-for-count (?j 1 (length ?list))
(bind ?element (nth ?j ?list))
(if (member ?element ?obras-sala)
then
(if (eq ?foundany FALSE)
then
(bind ?foundany TRUE)
(printout t "SALA " (send (send ?sala get-sala_escuela) get-nombre_escuela) ":" crlf)
(printout t crlf))
(printout t "   "(send ?element get-nombre_obra) " ")
(printout t "[VISIT TIME: " (nth ?j ?tiempos) " MINUTES]" crlf)))
(if (eq ?foundany TRUE)
then
(printout t crlf))
))

;regla que mostrara por pantalla la visita sugerida para el dia 3
(defrule print-visit3 ""
(declare (salience -7))
(visita-recortada3 $?list)
(tiempo-visita3 $?tiempos)
=>
(bind ?counter 0)
(loop-for-count (?k 1 (length ?tiempos))
(bind ?arttime (nth ?k ?tiempos))
(bind ?counter (+ ?counter ?arttime)))
(printout t  crlf)
(printout t "--------------")
(printout t "-- Day Three: --")
(printout t "--------------")
(printout t "     VISIT TIME: " ?counter)
(printout t crlf crlf)
(bind ?salas (find-all-instances ((?inst SALA)) TRUE))
(loop-for-count (?i 1 (length ?salas))
(bind ?sala (nth ?i ?salas))
(bind ?obras-sala (send ?sala get-sala_obra))
(bind ?foundany FALSE)
(loop-for-count (?j 1 (length ?list))
(bind ?element (nth ?j ?list))
(if (member ?element ?obras-sala)
then
(if (eq ?foundany FALSE)
then
(bind ?foundany TRUE)
(printout t "SALA " (send (send ?sala get-sala_escuela) get-nombre_escuela) ":" crlf)
(printout t crlf))
(printout t "   "(send ?element get-nombre_obra) " ")
(printout t "[VISIT TIME: " (nth ?j ?tiempos) " MINUTES]" crlf)))
(if (eq ?foundany TRUE)
then
(printout t crlf))
))

;regla que mostrara por pantalla una pequeña justificacion del porque se han asignado esas obras en la visita y no otras
(defrule print-justification ""
(declare (salience -10))
(visita-recortada $?list)
(cultural-level ?knowledge)
(child-presence ?child)
(painter-preferences $?painters)
(theme-preferences $?themes)
(movement-preferences $?movements)
(period-preferences $?periods)
=>
(printout t "-------------------------------------------" crlf)
(printout t crlf)
(printout t "-------------- Justification --------------" crlf)
(printout t crlf)
(if (eq ?child TRUE)
then
(printout t "- As you are visiting our museum with children, your visit(s) duration has been shortened to short session(s)" crlf )
(printout t "- Also as you are visiting our museum with children, works of art containing adult content have been eliminated from the visit" crlf crlf)
)
(if (eq ?knowledge little)
then
(printout t "- As a beginner, low complexity works of art have been prioritized, ")
)
(if (eq ?knowledge intermediate)
then
(printout t "- As an intermediate, intermediate complexity works of art have been prioritized, ")
)
(if (eq ?knowledge expert)
then
(printout t "- As an expert, high complexity works of art have been prioritized, ")
)
(printout t "so you can enjoy more the experience in the museum" crlf crlf)
(if (not(member NONE $?painters))
then
(printout t "- The works of art of the artists ")
(loop-for-count (?i 1 (length ?painters))
(bind ?painter (nth ?i ?painters))
(printout t (send ?painter get-nombre_autor) ", ")
)
(printout t "have been prioritized" crlf crlf)
)
(if (not(member NONE $?themes))
then
(printout t "- The works of art of the themes ")
(loop-for-count (?i 1 (length ?themes))
(bind ?theme (nth ?i ?themes))
(printout t (send ?theme get-nombre_tematica) ", ")
)
(printout t "have been prioritized" crlf crlf)
)
(if (not(member NONE $?movements))
then
(printout t "- The works of art of the movements ")
(loop-for-count (?i 1 (length ?movements))
(bind ?movement (nth ?i ?movements))
(printout t (send ?movement get-nombre_escuela) ", ")
)
(printout t "have been prioritized" crlf crlf)
)
(if (not(member NONE $?periods))
then
(printout t "- The works of art of the periods ")
(loop-for-count (?i 1 (length ?periods))
(bind ?period (nth ?i ?periods))
(printout t (send ?period get-nombre_epoca) ", ")
)
(printout t "have been prioritized" crlf crlf)
)
(printout t "- Overall, the visit prioritizes works of art with a higher importance in their historical and artistic context" crlf crlf)
)
