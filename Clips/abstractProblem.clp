;;;***************
;;;* SPECIFIC PROBLEM AUXILIAR FUNCTIONS *
;;;***************

;funcion que escribe por pantalla todos los nombres de los autores que se pueden encontrar en nuestro museo, se utiliza para preguntar la preferencia de autor
(deffunction print-painters ()
  (bind ?recorrido (find-all-instances ((?inst AUTOR)) TRUE))
  (loop-for-count (?i 1 (length ?recorrido))
  (bind ?element (nth ?i ?recorrido))
  (if (not (eq (send ?element get-nombre_autor) "UNDEFINED"))
  then
  (printout t ?i ": " (send ?element get-nombre_autor) crlf)))
  (length ?recorrido)
  )

;funcion que escribe por pantalla todos los movimientos que se pueden encontrar en nuestro museo, se utiliza para preguntar la preferencia de movimientos
(deffunction print-movements ()
  (bind ?recorrido (find-all-instances ((?inst ESCUELA)) TRUE))
  (loop-for-count (?i 1 (length ?recorrido))
  (bind ?element (nth ?i ?recorrido))
  (if (not (eq (send ?element get-nombre_escuela) "NO ESCUELA"))
  then
  (printout t ?i ": " (send ?element get-nombre_escuela) crlf)))
  (length ?recorrido)
  )

;funcion que escribe por pantalla todas las tematicas que se pueden encontrar en nuestro museo, se utiliza para preguntar la preferencia de tematicas
(deffunction print-themes ()
  (bind ?recorrido (find-all-instances ((?inst TEMATICA)) TRUE))
  (loop-for-count (?i 1 (length ?recorrido))
  (bind ?element (nth ?i ?recorrido))
  (printout t ?i ": " (send ?element get-nombre_tematica) crlf))
  (length ?recorrido)
  )

;funcion que escribe por pantalla todos los periodos que se pueden encontrar en nuestro museo, se utiliza para preguntar la preferencia de periodos
(deffunction print-periods ()
  (bind ?recorrido (find-all-instances ((?inst EPOCA)) TRUE))
  (loop-for-count (?i 1 (length ?recorrido))
  (bind ?element (nth ?i ?recorrido))
  (printout t ?i ": " (send ?element get-nombre_epoca) crlf))
  (length ?recorrido)
  )

;funcion que sirve para hacer una pregunta(pregunta) que puede recibir como maximo tres enteros que no sean mayores de max-allowed-value ni menores de 1, se utiliza para recibir las preferencias
(deffunction multiple-numeric-question (?pregunta ?max-allowed-value)
  (bind ?correctValues FALSE)
  (while (eq ?correctValues FALSE) do
    (format t "%s" ?pregunta)
    (bind ?resposta (readline))
    (bind ?res (str-explode ?resposta))
    (bind ?correctValues TRUE)
    (bind ?i 1)
    ;(while (and (<= ?i (length $?res)) (eq ?correctValues TRUE)) do
    (while (and (<= ?i (length $?res)) (eq ?correctValues TRUE)) do
      (bind ?autor (nth $?i ?res))
      (if (not (and (>= ?autor 1) (<= ?autor ?max-allowed-value)))
      then (bind ?correctValues FALSE))
      (bind ?i (+ ?i 1)))
    (if (> ?i 4)
    then (bind ?correctValues FALSE)))
    ?res)

;;;***************
;;;* SPECIFIC PROBLEM ASSERTIONS RULES
;;;***************

;regla que determinara el tamaño del grupo que va a visitar el museo
(defrule determine-group-size ""
    (declare (salience 0))
    (not (group-size ?))
    (not (visit ?))
    =>
    (if (yes-or-no-p "Is the visit performed by a single person? (yes/no)? ")
        then
        (assert (group-size single))
        else
        (bind ?response
          (ask-question "Is the visit performed by a couple (2 people), a small group (10 people or less) or a big group (more than 10 people) (couple/small/big)? " couple small big))
        (if (eq ?response couple)
          then
          (assert (group-size couple)))
        (if (eq ?response small)
          then
          (assert (group-size small)))
        (if (eq ?response big)
          then
          (assert (group-size big)))))

;regla que determinara el nivel de arte del grupo que va a visitar el museo
(defrule determine-cultural-level ""
    (declare (salience 0))
    (group-size ?)
    (not (cultural-level ?))
    (not (visit ?))
    =>
    (bind ?response
      (ask-question "What is the overall art-knowledge of the visitor(s)? (little/intermediate/expert)? " little intermediate expert ))
    (if (eq ?response little)
      then
      (assert (cultural-level little)))
    (if (eq ?response intermediate)
      then
      (assert (cultural-level intermediate)))
    (if (eq ?response expert)
      then
      (assert (cultural-level expert))))

;regla que determinara el numero de dias de la visita y las horas de la visita de cada dia
(defrule determine-visit-duration ""
    (declare (salience 0))
    (not (days-duration ?))
    (not (hours-duration ?))
    (cultural-level ?cult)
    (not (visit ?))
    =>
    (bind ?response
      (ask-question "How many days/sessions would you like your visit to last? (one/two/three) " one two three ))
    (if (eq ?response one)
      then
      (assert (days-duration one)))
    (if (eq ?response two)
      then
      (assert (days-duration two)))
    (if (eq ?response three)
      then
      (assert (days-duration three)))
    (if (eq ?cult little)
      then
      (printout t crlf crlf)
      (printout t "BEGINNER, PLEASE BEAR IN MIND: LONG SESSIONS ARE NOT RECOMMENDED FOR PEOPLE WITH LITTLE ART KNOWLEDGE, TAKE IT EASY, CHOOSE SHORT SESSIONS AND YOU'LL ENJOY MORE!")
      (printout t crlf crlf))
    (bind ?response
      (ask-question "How long do you want the visit(s) to be, short (aprox. 1 hour), intermediate (2-3 hours), long (aprox. 4 hours)? (short/intermediate/long) " short intermediate long ))
    (if (eq ?response short)
      then
      (assert (hours-duration short)))
    (if (eq ?response intermediate)
      then
      (assert (hours-duration intermediate)))
    (if (eq ?response long)
      then
      (assert (hours-duration long))))

;regla que determinara la existencia de niños en la visita, en el caso que la visita se haga por una sola persona, dicha persona no puede ser un niño
(defrule determine-child-presence ""
    (declare (salience -1))
    (group-size ?size)
    (not (child-presence ?))
    (not (visit ?))
    =>
    (if (eq ?size single)
      then
      (assert (child-presence FALSE))
      else
      (if (yes-or-no-p "Are there going to be children in the visit? (yes/no)? ")
          then
          (assert (child-presence TRUE))
          else
          (assert (child-presence FALSE)))))

;regla que determinara si los visitantes tienen preferencia por algun autor
(defrule determine-painter-preferences ""
      (declare (salience 0))
      (child-presence ?)
      (not (painter-preferences ?))
      =>
      (bind ?autores (find-all-instances ((?inst AUTOR)) TRUE))
      (if (yes-or-no-p "Are you specially interested to visit the work of up to 3 selected artists at our museum? (yes/no) ")
          then
          (bind ?size (print-painters))
          (bind ?response
            (multiple-numeric-question "Which artist's work do you want to visit at our museum? Choose up to 3 options:
" ?size))
            (bind $?lista-autores (create$ ))
            (loop-for-count (?i 1 (length ?response))
            (bind ?indice (nth ?i ?response))
            (bind ?autor (nth ?indice ?autores))
            (bind ?lista-autores (insert$ $?lista-autores ?i ?autor)))
            (assert (painter-preferences ?lista-autores))
          else
          (assert (painter-preferences NONE))))

;regla que determinara si los visitantes tienen preferencia por alguna tematica
(defrule determine-theme-preferences ""
      (declare (salience 0))
      (not (theme-preferences ?))
      (painter-preferences $?)
      =>
      (bind ?temas (find-all-instances ((?inst TEMATICA)) TRUE))
      (if (yes-or-no-p "Are you interested in visiting pieces of art about up to 3 themes? (yes/no) ")
          then
          (bind ?size(print-themes))
          (bind ?response
            (multiple-numeric-question "Which theme(s) do you want to explore at your visit? Choose up to 3 options:
" ?size))
            (bind $?lista-temas (create$ ))
            (loop-for-count (?i 1 (length ?response))
            (bind ?indice (nth ?i ?response))
            (bind ?tematica (nth ?indice ?temas))
            (bind ?lista-temas (insert$ $?lista-temas ?i ?tematica)))
            (assert (theme-preferences ?lista-temas))
          else
          (assert (theme-preferences NONE))))

;regla que determinara si los visitantes tienen preferencia por algun movimiento (escuela)
(defrule determine-movement-preferences ""
      (declare (salience 0))
      (not (movement-preferences ?))
      (theme-preferences $?)
      =>
      (bind ?escuelas (find-all-instances ((?inst ESCUELA)) TRUE))
      (if (yes-or-no-p "Are you interested in visiting pieces of art about up to 3 modern art movements? (yes/no) ")
          then
          (bind ?size (print-movements))
          (bind ?response
            (multiple-numeric-question "Which art movement(s) do you want to explore at your visit? Choose up to 3 options:
" ?size))
            (bind $?lista-escuelas (create$ ))
            (loop-for-count (?i 1 (length ?response))
            (bind ?indice (nth ?i ?response))
            (bind ?escuela (nth ?indice ?escuelas))
            (bind ?lista-escuelas (insert$ $?lista-escuelas ?i ?escuela)))
            (assert (movement-preferences ?lista-escuelas))
          else
          (assert (movement-preferences NONE))))

;regla que determinara si los visitantes tienen preferencia por algun periodo
(defrule determine-period-preferences ""
      (declare (salience 0))
      (not (period-preferences ?))
      (movement-preferences $?)
      =>
      (bind ?epocas (find-all-instances ((?inst EPOCA)) TRUE))
      (if (yes-or-no-p "Are you interested in visiting pieces of art about up to 3 periods? (yes/no) ")
          then
          (bind ?size (print-periods))
          (bind ?response
            (multiple-numeric-question "Which period(s) do you want to explore at your visit? Choose up to 3 options:
" ?size))
            (bind $?lista-epocas (create$ ))
            (loop-for-count (?i 1 (length ?response))
            (bind ?indice (nth ?i ?response))
            (bind ?epoca (nth ?indice ?epocas))
            (bind ?lista-epocas (insert$ $?lista-epocas ?i ?epoca)))
            (assert (period-preferences ?lista-epocas))
          else
          (assert (period-preferences NONE))))


;;;***************
;;;* ABSTRACT PROBLEM INCOMPATIBILITY RULES *
;;;***************

;regla que recorta la visita a una hora si hay niños y habiamos dicho de estar mas de una hora
(defrule shorten-visit-with-children ""
  (declare (salience 5))
  (child-presence TRUE)
  (hours-duration long | intermediate)
  ?vent <- (hours-duration ?)
  =>
  (retract ?vent)
  (assert (hours-duration short))
  (assert (child-shorten TRUE))
  (printout t crlf crlf)
  (printout t "AS YOU ARE VISITING OUR MUSEUM WITH A CHILDREN, YOUR VISIT(S) DURATION HAS BEEN SHORTENED TO SHORT SESSION(S)")
  (printout t crlf crlf))

;regla que recorta la visita si somos principiantes y queriamos venir cuatro horas durante tres dias, se recorta a dos/tres horas por dia
(defrule shorten-visit-for-beginner ""
  (declare (salience 5))
  (hours-duration long)
  (days-duration three)
  (cultural-level little)
  ?vent <- (hours-duration ?)
  =>
  (retract ?vent)
  (assert (hours-duration intermediate))
  (printout t crlf crlf)
  (printout t "AS YOU ARE A BEGINNER VISITING OUR MUSEUM , THE DURATION OF YOUR THREE VISITS HAS BEEN SHORTENED")
  (printout t crlf crlf))
