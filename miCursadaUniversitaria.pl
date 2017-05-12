%use_module(library(plunit)).
%load_test_files(tests.plt).

%LAS MATERIAS

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
	aproboFinal(Estudiante,Materia).

aprobo(Estudiante,Materia) :-
	aproboFinal(Estudiante, Materia).

aprobo(Estudiante,Materia) :-
	rindioLibre(Estudiante,Materia).

aprobo(Estudiante,Materia) :-
	promociono(Estudiante,Materia).

%4-----------------------------------------
aproboCursada(Estudiante, Materia) :-
	notaCursadaMayorALimite(Estudiante,Materia,_,4).

promociono(Estudiante,Materia) :-
	promocionable(Materia),
	notaCursadaMayorALimite(Estudiante,Materia,_,7).

notaCursadaMayorALimite(Estudiante,Materia,Nota,Limite) :-
	notaCursada(Estudiante, Materia, Nota),
	mayorIgual(Nota,Limite).

mayorIgual(Nota,Limite) :- Nota >= Limite.

aproboFinal(Estudiante, Materia) :-
	notaFinalMayorALimite(Estudiante,Materia,_,4).

notaFinalMayorALimite(Estudiante,Materia,Nota,Limite) :-
	notaFinal(Estudiante, Materia, Nota),
	mayorIgual(Nota,Limite).

debeElFinal(Estudiante,Materia):-
	curso(Estudiante,Materia),
	not(aprobo(Estudiante,Materia)).

bloquea(Estudiante,Materia,OtraMateria) :-
	sonNecesariasParaCursar(Materia, OtraMateria),
	debeElFinal(Materia),
	aproboCursada(Estudiante,OtraMateria).

perdioPromocion(Estudiante,Materia) :-
	promociono(Estudiante,Materia),
	sonNecesariasParaCursar(Materia,OtraMateria),
	debeElFinal(OtraMateria).

estaAlDia(Estudiante) :-
% ver si necesitamos un predicado generador
	esEstudiante(Estudiante),
	forall(curso(Estudiante,Materia), not(debeElFinal(Estudiante,Materia)).

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
esCorrelativaDe(matematicaII,electricidadYMagnetismo).
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

	test(algoritmosI_es_materia_Pesada):-
	esPesada(materia(algoritmosI,160)).

	test(basesDeDatos_es_materia_Pesada):-
	esPesada(materia(basesDeDatos,128)).

	test(metodosNumericos_NO_es_materia_Pesada,fail):-
	esPesada(materia(metodosNumericos,80)).

:- end_tests(materias_pesadas).

:- begin_tests(materias_iniciales).

	test(matematicaI_es_materia_inicial):-
	materiaInicial(matematicaI).

	test(laboratorioDeComputacionI_es_materia_inicial):-
	materiaInicial(laboratorioDeComputacionI).

	test(electricidadYMagnetismo_es_materia_inicial):-
	materiaInicial(electricidadYMagnetismo).

:- end_tests(materias_iniciales).

:- begin_tests(materias_necesarias_para_cursar).

	test(algoritmosI_requiere_matematicaIyII_laboratorioIyII_spd):-
	sonNecesariasParaCursar(algoritmosI,matematicaI),
	sonNecesariasParaCursar(algoritmosI,matematicaII),
	sonNecesariasParaCursar(algoritmosI,laboratorioDeComputacionI),
	sonNecesariasParaCursar(algoritmosI,laboratorioDeComputacionII),
	sonNecesariasParaCursar(algoritmosI,spd).

:- end_tests(materias_necesarias_para_cursar).

%7-----------------------------------------


%8-----------------------------------------