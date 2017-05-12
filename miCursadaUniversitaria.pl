%use_module(library(plunit)).
%load_test_files(tests.plt).

%1-----------------------------------------
esPesada(materia(_,HorasTotales)):-
	HorasTotales > 100.

esPesada(materia(Nombre,_)):-
    tieneNombreCorto(Nombre),
    not(promocionable(Nombre)).

tieneNombreCorto(Nombre):-
	atom_length(Nombre,X),
	X =< 15.

%2-----------------------------------------
materiaInicial(Nombre):-
	esMateria(materia(Nombre,_)),
	not(esCorrelativaDe(Nombre,_)).

sonNecesariasParaCursar(Materia, Correlativa) :-
	esCorrelativaDe(Materia, OtraMateria),
	sonNecesariasParaCursar(OtraMateria, Correlativa).
sonNecesariasParaCursar(Materia, Correlativa) :-
	esCorrelativaDe(Materia, Correlativa).

materiasQueHabilita(Correlativa, Materia) :-
	sonNecesariasParaCursar(Materia,Correlativa).

%LOS ESTUDIANTES

%3-----------------------------------------
curso(Estudiante,Materia) :-
	aproboCursada(Estudiante, Materia).

curso(Estudiante,Materia) :-
	rindioLibre(Estudiante, Materia).

rindioLibre(Estudiante,Materia) :-
	not(aproboCursada(Estudiante,Materia)),
	aproboFinal(Estudiante,Materia).

aprobo(Estudiante,Materia) :-
	aproboFinal(Estudiante, Materia).

aprobo(Estudiante,Materia) :-
	rindioLibre(Estudiante,Materia).

aprobo(Estudiante,Materia) :-
	promociono(Estudiante,Materia).

%4-----------------------------------------
aproboCursada(Estudiante, Materia) :-
	notaCursada(Estudiante, Materia, Nota),
	notaAprobacion(Nota).

promociono(Estudiante,Materia) :-
	promocionable(Materia),
	notaCursada(Estudiante, Materia, Nota),
	notaPromocion(Nota).

notaPromocion(Nota) :- Nota >= 7.
notaAprobacion(Nota) :- Nota >= 4.

aproboFinal(Estudiante, Materia) :-
	notaFinal(Estudiante,Materia,Nota),
	notaAprobacion(Nota).

debeElFinal(Estudiante,Materia) :-
	curso(Estudiante,Materia),
	not(aprobo(Estudiante,Materia)).

bloquea(Estudiante,Materia,OtraMateria) :-
	sonNecesariasParaCursar(Materia, OtraMateria),
	debeElFinal(Estudiante,Materia),
	aproboCursada(Estudiante,OtraMateria).

perdioPromocion(Estudiante,Materia) :-
	promociono(Estudiante,Materia),
	sonNecesariasParaCursar(Materia,OtraMateria),
	debeElFinal(Estudiante,OtraMateria).

estaAlDia(Estudiante) :-
  	esEstudiante(Estudiante),
	forall(curso(Estudiante,Materia), not(debeElFinal(Estudiante,Materia))).

esEstudiante(Estudiante) :-
	notaCursada(Estudiante,_,_).

%CASOS DE PRUEBA

%5-----------------------------------------

esMateria(materia(matematicaII,96)).
esMateria(materia(matematicaI,96)).
esMateria(materia(matematicaIII,96)).
esMateria(materia(laboratorioDeComputacionI,128)).
esMateria(materia(laboratorioDeComputacionII,128)).
esMateria(materia(electricidadYMagnetismo,128)).
esMateria(materia(spd,128)).
esMateria(materia(algoritmosI,160)).
esMateria(materia(sistemasOperativos,96)).
esMateria(materia(algoritmosII,160)).
esMateria(materia(algoritmosIII,160)).
esMateria(materia(redesLocales,128)).
esMateria(materia(metodosNumericos,80)).
esMateria(materia(basesDeDatos,128)).
esMateria(materia(seminarioDeProgramacion,64)).
esMateria(materia(phm,160)).
esMateria(materia(proyectoDeSoftware,128)).
esMateria(materia(pdp,64)).
esCorrelativaDe(matematicaII,matematicaI).
esCorrelativaDe(matematicaII,laboratorioDeComputacionI).
esCorrelativaDe(laboratorioDeComputacionII,laboratorioDeComputacionI).
esCorrelativaDe(spd,laboratorioDeComputacionI).
esCorrelativaDe(matematicaIII,matematicaII).
esCorrelativaDe(matematicaIII,laboratorioDeComputacionII).
esCorrelativaDe(algoritmosI,laboratorioDeComputacionII).
esCorrelativaDe(algoritmosI,matematicaII).
esCorrelativaDe(algoritmosI,spd).
esCorrelativaDe(sistemasOperativos,laboratorioDeComputacionII).
esCorrelativaDe(sistemasOperativos,spd).
esCorrelativaDe(algoritmosII,matematicaIII).
esCorrelativaDe(algoritmosII,algoritmosI).
esCorrelativaDe(metodosNumericos,algoritmosI).
esCorrelativaDe(redesLocales,sistemasOperativos).
esCorrelativaDe(basesDeDatos,algoritmosII).
esCorrelativaDe(algoritmosIII,algoritmosII).
esCorrelativaDe(algoritmosIII,redesLocales).
esCorrelativaDe(seminarioDeProgramacion,algoritmosII).
esCorrelativaDe(seminarioDeProgramacion,metodosNumericos).
esCorrelativaDe(seminarioDeProgramacion,redesLocales).
esCorrelativaDe(proyectoDeSoftware,algoritmosIII).
esCorrelativaDe(proyectoDeSoftware,basesDeDatos).
esCorrelativaDe(phm,algoritmosIII).
esCorrelativaDe(pdp,algoritmosIII).
promocionable(algoritmosI).
promocionable(laboratorioDeComputacionI).
promocionable(laboratorioDeComputacionII).
promocionable(matematicaI).
promocionable(matematicaII).
promocionable(electricidadYMagnetismo).
promocionable(spd).
promocionable(sistemasOperativos).
promocionable(pdp).

%6-----------------------------------------

:- begin_tests(materias_pesadas).

	test(algoritmosI_es_materia_Pesada, nondet) :-
	esPesada(materia(algoritmosI,160)).

	test(basesDeDatos_es_materia_Pesada, nondet) :-
	esPesada(materia(basesDeDatos,128)).

	test(metodosNumericos_NO_es_materia_Pesada,fail) :-
	esPesada(materia(metodosNumericos,80)).

:- end_tests(materias_pesadas).

:- begin_tests(materias_iniciales).

	test(matematicaI_es_materia_inicial, nondet) :-
	materiaInicial(matematicaI).

	test(laboratorioDeComputacionI_es_materia_inicial, nondet) :-
	materiaInicial(laboratorioDeComputacionI).

	test(electricidadYMagnetismo_es_materia_inicial, nondet) :-
	materiaInicial(electricidadYMagnetismo).

:- end_tests(materias_iniciales).

:- begin_tests(materias_necesarias_para_cursar).

	test(algoritmosI_requiere_matematicaIyII_laboratorioIyII_spd, nondet) :-
	sonNecesariasParaCursar(algoritmosI,matematicaI),
	sonNecesariasParaCursar(algoritmosI,matematicaII),
	sonNecesariasParaCursar(algoritmosI,laboratorioDeComputacionI),
	sonNecesariasParaCursar(algoritmosI,laboratorioDeComputacionII),
	sonNecesariasParaCursar(algoritmosI,spd).

:- end_tests(materias_necesarias_para_cursar).

%7-----------------------------------------
notaCursada(pepo,electricidadYMagnetismo,8).
notaCursada(pepo,matematicaI,8).
notaCursada(pepo,laboratorioDeComputacionI,8).

notaCursada(pepo,laboratorioDeComputacionII, 5).
notaCursada(pepo,matematicaII, 6).
notaCursada(pepo,matematicaIII, 4).

notaFinal(pepo,matematicaII,4).
notaFinal(pepo,laboratorioDeComputacionII,2).
notaFinal(pepo,spd,6).

%8-----------------------------------------
:- begin_tests(pepo).

test(aprobo_labo2_pepo, fail):-
	aprobo(pepo,laboratorioDeComputacionII).

test(aprobo_mateI_pepo, nondet):-
	aprobo(pepo,matematicaI).

test(aprobo_mateII_pepo, nondet):-
	aprobo(pepo,matematicaII).

test(aprobo_electricidad_pepo, nondet):-
	aprobo(pepo,electricidadYMagnetismo).

test(aprobo_spd_pepo, nondet):-
	aprobo(pepo,spd).

test(no_esta_al_dia_pepo, fail):-
	estaAlDia(pepo).

test(perdio_promocion_pepo, fail):-
	perdioPromocion(pepo,_).

test(bloquea_labo2_a_mate3,
     set(Materia == [laboratorioDeComputacionII])):-
	bloquea(pepo,matematicaIII,Materia).

:- end_tests(pepo).
