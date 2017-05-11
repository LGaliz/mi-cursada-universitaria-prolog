use_module(library(plunit)).
load_test_files(tests.plt).
%Base de Conocimiento
%materia(Nombre,HorasTotalesCursada).
%materia(phm,604).
%materia(pdp,10).
correlativa(matematicaII,matematicaIII).
promocionable(algoritmosI).
promocionable(laboratorioDeComputacionI).
promocionable(laboratorioDeComputacionII).
promocionable(matematicaI).
promocionable(matematicaII).
promocionable(electricidadYMagnetismo).
promocionable(spd).
promocionable(sistemasOperativos).
promocionable(pdp).

% materia(algoritmosI tiene que tener mas de 100 horas
% materia(basesDeDatos, 120).
%Las materias iniciales de la carrera son: Matemática I, Laboratorio de Computación I, Electricidad y Magnetismo.
%Las materias totales necesarias para cursar Algoritmos I son: Matemática I y II, Laboratorio de Computación I y II, Sistemas de Pr%ocesamiento de Datos.

%Las Materias

%1-----------

esPesada(materia(_,HorasTotales)):-
	HorasTotales > 100.

esPesada(materia(Nombre,_)):-
    tieneNombreCorto(Nombre),
    not(promocionable(Nombre)).

tieneNombreCorto(Nombre):-
	atom_length(Nombre,X),
	X =< 15.

esMateria(materia(matematicaII,30)). % hacer esto con todas
esMateria(materia(matematicaI,10)). % hacer esto con todas

materiaInicial(Nombre):-
	esMateria(materia(Nombre,_)),
	not(esCorrelativaDe(Nombre,_)).

esCorrelativaDe(matematicaII,matematicaI).
esCorrelativaDe(matematicaIII,matematicaII).
esCorrelativaDe(laboratorio2,matematicaI).

materiasNecesariasParaCursar(Materia, Correlativa) :-
  esCorrelativaDe(Materia, OtraMateria),
  materiasNecesariasParaCursar(OtraMateria, Correlativa).

materiasNecesariasParaCursar(Materia, Correlativa) :-
  esCorrelativaDe(Materia, Correlativa).

% Aca no juega la transitividad vieji
materiasQueHabilita(Correlativa, Materia) :-
  esCorrelativaDe(Materia, Correlativa).


				% Punto 3
curso(Estudiante,Materia) :-
	aproboCursada(Estudiante, Materia).

curso(Estudiante,Materia) :-
	rindioLibre(Estudiante, Materia).

aprobo(Estudiante,Materia) :-
	aproboFinal(Estudiante, Materia).

aprobo(Estudiante,Materia) :-
	rindioLibre(Estudiante,Materia).

aprobo(Estudiante,Materia) :-
	promociono(Estudiante,Materia).

aproboCursada(Estudiante, Materia) :-
	notaCursadaMayorALimite(Estudiante,Materia,Nota,4).

promociono(Estudiante,Materia) :-
	promocionable(Materia),
	notaCursadaMayorALimite(Estudiante,Materia,Nota,7).

notaCursadaMayorALimite(Estudiante,Materia,Nota,Limite) :-
	notaCursada(Estudiante, Materia, Nota),
	mayorIgual(Nota,Limite).

mayorIgual(Nota,Limite) :- Nota >= Limite.