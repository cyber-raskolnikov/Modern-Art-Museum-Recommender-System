
;;;======================================================
;;;   Museum Visit Recomendator Expert System
;;;
;;;     This expert system recommends art pieces for a museum visit.
;;;
;;;     CLIPS Version 6.0 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* FUNCTIONS *
;;****************

;funcion para cargar la ontologia y todos los demas ficheros. Cuando termina de cargar todo muestra un mensaje de bienvenida.
(deffunction initialization ()
  (load "../Protege/ontologia.pont")
  (load-instances "../Protege/ontologia.pins")
  (load "./abstractSolution.clp")
  (load "./abstractProblem.clp")
  (load "./specificSolution.clp")
  (load "./refinamiento.clp")
  (printout t crlf crlf)
  (printout t "The Art Recommendation Expert System")
  (printout t crlf crlf))

;funcion para hacer una pregunta(question) que debe recibir una respuesta esperada (allowed-values)
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer)

;funcion para hacer una pregunta(pregunta) que puede recibir mas de una respuesta, pero siempre tiene que ser respuestas permitidas(allowed-values)
(deffunction multiple-question (?pregunta $?allowed-values)
  (bind ?correctValues FALSE)
  (while (eq ?correctValues FALSE) do
    (format t "%s" ?pregunta)
    (bind ?resposta (readline))
    (bind ?res (str-explode ?resposta))
    (bind ?correctValues TRUE)
    (bind ?i 1)
    (while (and (<= ?i (length $?res)) (eq ?correctValues TRUE)) do
      (bind ?autor (nth $?i ?res))
      (if (not (member ?autor ?allowed-values))
      then (bind ?correctValues FALSE))
      (bind ?i (+ ?i 1))))
    ?res)

;funcion para hacer una pregunta(question) que debe ser contestada con un si o un no
(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE
       else FALSE))


;;;****************************
;;;* STARTUP AND FINAL VISIT RULES *
;;;****************************

;primera regla que se ejecuta, carga todos los archivos necesarios
(defrule startup ""
 (declare (salience 10))
 =>
 (initialization)
 (assert (time-list (create$ 7 8 10 12 10 12 15 20 15 20 25 30 7 8 10 12 12 15 20 25 20 25 30 35 7 8 12 12 15 20 25 30 25 30 35 40 ))))
