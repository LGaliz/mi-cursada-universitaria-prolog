use_module(library(plunit)).
load_test_files(tests.plt).
%Base de Conocimiento
%materia(Nombre,HorasTotalesCursada).
%materia(phm,604).
%materia(pdp,10).
promocionable(pdp).
correlativa(matematicaII,matematicaIII).

promocionable(algoritmosI).
promocionable(laboratorioDeComputacionI).
promocionable(laboratorioDeComputacionII)
promocionable(matematicaI).
promocionable(matematicaII).
promocionable(electricidadYMagnetismo).
promocionable(spd).
promocionable(sistemasOperativos).
promocionable(pdp).

% materia(algoritmosI tiene que tener mas de 100 horas
% materia(basesDeDatos, 120).
Las materias iniciales de la carrera son: Matemática I, Laboratorio de Computación I, Electricidad y Magnetismo.
Las materias totales necesarias para cursar Algoritmos I son: Matemática I y II, Laboratorio de Computación I y II, Sistemas de Procesamiento de Datos.

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

 %forall(quiereIr(Persona, Destino), quedaEn(Destino, Zona)).


materiaInicial(Materia):-
	not(correlativa(Materia,_)).

materiasNecesariasParaCursar(Materia):-
	correlativa(Materia,Materia2).
materiasNecesariasParaCursar(Materia):-
	correlativa(Materia,Materia2),
	correlativa(Materia2,Materia3).


				%ancestro(Persona, Ancestro):-
				%	padre(Persona, Ancestro).
				%ancestro(Persona, Ancestro):-
				%	padre(Persona, Padre),
				%	ancestro(Padre, Ancestro).
