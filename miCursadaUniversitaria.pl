%PARTE 1

%Base de Conocimiento
%materia(Nombre,HorasTotalesCursada).
materia(paradigmasDeProgramacion,64).
materia(pdp,10).
promocionable(phm).
correlativa(matematicaI(10),matematicaII).
correlativa(matematicaII,matematicaIII).

%Las Materias

%1-----------

esPesada(Materia,HorasTotales):-
	HorasTotales >100.
	
esPesada2(materia(Materia,HorasTotales)):-
	HorasTotales >100.
	
esPesada(Materia,HorasTotales):-
	%tieneNombreCorto(Materia),
    not(promocionable(Materia)).
	
% tieneNombreCorto(Materia):-
	% nombreConMasDeQuinceLetras(Materia).
	
	%atom_length
%2------------

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
	