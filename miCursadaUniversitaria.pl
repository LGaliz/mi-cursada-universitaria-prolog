%1 Parte1----------------------------------
esPesada(materia(_,HorasTotales)):-
	HorasTotales > 100.

esPesada(materia(Nombre,_)):-
    tieneNombreCorto(Nombre),
    not(promocionable(Nombre)).

tieneNombreCorto(Nombre):-
	atom_length(Nombre,X),
	X =< 15.

%2 Parte1----------------------------------
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

%3 Parte1----------------------------------
curso(Estudiante,Materia) :-
	aproboCursada(Estudiante, Materia).

curso(Estudiante,Materia) :-
	rindioLibre(Estudiante, Materia).

rindioLibre(Estudiante,Materia) :-
	aproboFinal(Estudiante,Materia),
	not(aproboCursada(Estudiante,Materia)).

aprobo(Estudiante,Materia) :-
	aproboFinal(Estudiante, Materia).

aprobo(Estudiante,Materia) :-
	rindioLibre(Estudiante,Materia).

aprobo(Estudiante,Materia) :-
	promociono(Estudiante,Materia).

%4 Parte1----------------------------------
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
	debeElFinal(Estudiante,OtraMateria),
	aproboCursada(Estudiante,Materia).

perdioPromocion(Estudiante,Materia) :-
	promociono(Estudiante,Materia),
	sonNecesariasParaCursar(Materia,OtraMateria),
	debeElFinal(Estudiante,OtraMateria).

estaAlDia(Estudiante) :-
  	esEstudiante(Estudiante),
	forall(curso(Estudiante,Materia), not(debeElFinal(Estudiante,Materia))).

esEstudiante(Estudiante) :-
	notaCursada(Estudiante,_,_).

%5 Parte1----------------------------------
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

%7 Parte1----------------------------------
notaFinal(pepo,matematicaII,4).
notaFinal(pepo,laboratorioDeComputacionII,2).
notaFinal(pepo,spd,6).

notaCursada(pepo,electricidadYMagnetismo,8).
notaCursada(pepo,matematicaI,8).
notaCursada(pepo,laboratorioDeComputacionI,8).
notaCursada(pepo,laboratorioDeComputacionII, 5).
notaCursada(pepo,matematicaII, 6).
notaCursada(pepo,matematicaIII, 4).

%9 Parte2----------------------------------
notaCursada(lescano,electricidadYMagnetismo,9).
notaCursada(lescano,matematicaI,8).	
notaCursada(lescano,laboratorioDeComputacionI,10).

%UsuarioTest Parte2------------------------
notaCursada(mas,laboratorioDeComputacionI,10).
notaCursada(mas,matematicaI,8).	
notaCursada(mas,electricidadYMagnetismo,9).		
notaCursada(mas,matematicaII,10).

%1 Parte2----------------------------------
puedeCursar(Estudiante,Materia):-
	esMateria(materia(Materia,_)),
	not(curso(Estudiante,Materia)),
	cursoTodasLasMateriasParaCursar(Materia,Estudiante),
	regimenCorrelativas(Estudiante,Materia).
	
regimenCorrelativas(Estudiante,Materia):-
	aprobadasComoMinimoElSegundoNivelDeCorrelativas(Materia,Estudiante).
	
cursoTodasLasMateriasParaCursar(Materia,Estudiante):-
		forall(sonNecesariasParaCursar(Materia,Correlativa),curso(Estudiante,Correlativa)).

aprobadasComoMinimoElSegundoNivelDeCorrelativas(Materia,Estudiante):-
		forall(esMateriaDeSegundoNivel(Materia, MateriaSegundoNivel),aprobo(Estudiante,MateriaSegundoNivel)).
	
esMateriaDeSegundoNivel(Materia,MateriaSegundoNivel):-
	esCorrelativaDe(Materia, CorrelativaPrimerNivel),
	esCorrelativaDe(CorrelativaPrimerNivel, MateriaSegundoNivel),
	CorrelativaPrimerNivel \= MateriaSegundoNivel.

%2 Parte2------------------------------------------
enQueCuatrimestreCurso(Estudiante,Materia,Cuatrimestre,Anio):-
	fechaDeCursada(Estudiante,Materia,Cuatrimestre,Anio).

materiasRecursadas(Estudiante,Materia):-
		esMateria(materia(Materia,_)),
		esMateria(materia(OtraMateria,_)),
		enQueCuatrimestreCurso(Estudiante,Materia,Temporalidad,Anio),
		enQueCuatrimestreCurso(Estudiante,OtraMateria,OtraTemporalidad,OtroAnio),
		Materia==OtraMateria,
		momentosDistintos(Temporalidad,OtraTemporalidad,Anio,OtroAnio).

momentosDistintos(Temporalidad,OtraTemporalidad,_,_):-
	Temporalidad \= OtraTemporalidad.
momentosDistintos(_,_,Anio,OtroAnio):-
	Anio \= OtroAnio.	
	
%4 Parte2------------------------------------------

%Aca se repite codigo pero tengo mucho suenio 4 am  +0!!!
cursadaCuatrimestral(Estudiante,Materia):-
	fechaDeCursada(Estudiante,Materia,Temporalidad,Anio),
	fechaDeCursada(Estudiante,Materia,OtraTemporalidad,OtroAnio),
	Temporalidad \= OtraTemporalidad,
	restriccionCuatrimestral(Temporalidad,OtraTemporalidad,Anio,OtroAnio).
	
	restriccionCuatrimestral(primerCuatrimestre,segundoCuatrimestre,Anio,OtroAnio):-
	Anio == OtroAnio.
	restriccionCuatrimestral(segundoCuatrimestre,primerCuatrimestre,Anio,OtroAnio):-
	Anio is OtroAnio - 1.
	
cursadaVerano(Estudiante,Materia):-
	fechaDeCursada(Estudiante,Materia,verano(_,_),Anio),
	fechaDeCursada(Estudiante,Materia,verano(_,_),OtroAnio),
	Anio is OtroAnio - 1.

cursadaAnual(Estudiante,Materia):-
	fechaDeCursada(Estudiante,Materia,anual,Anio),
	fechaDeCursada(Estudiante,Materia,anual,OtroAnio),
	Anio is OtroAnio - 1.
	
perfil(sinDescanso(Estudiante)):-
	esEstudiante(Estudiante),
	esMateria(materia(Materia,_)),
	materiasRecursadas(Estudiante,Materia),
	%cursadaCuatrimestral(Estudiante,Materia),
	%cursadaAnual(Estudiante,Materia),
	cursadaVerano(Estudiante,Materia).
	
perfil(invictus(Estudiante)):-
	esEstudiante(Estudiante),
	not(materiasRecursadas(Estudiante,_)).
	
perfil(repechaje(Estudiante)):-
	esEstudiante(Estudiante),
	esMateria(materia(Materia,_)),
	materiasRecursadas(Estudiante,Materia),
	fechaDeCursada(Estudiante,Materia,anual,Anio),
	fechaDeCursada(Estudiante,Materia,anual,OtroAnio),	%todas las materias anuales empiezan en el primerCuatrimestre
	OtroAnio is Anio +1,
	promociono(Estudiante,Materia).
	
perfil(buenasCursadas(Estudiante)):-
	esEstudiante(Estudiante),
	esMateria(materia(Materia,_)),
	forall(promocionable(Materia),promociono(Estudiante,Materia)).
	
perfil(seLoQueHiciseElVeranoPasado(Estudiante)):-
	cursoTodosLosVeranos(Estudiante).
	
cursoTodosLosVeranos(Estudiante):-
	esMateria(materia(Materia,_)),
	forall(anioDeCursada(Estudiante,Anio),cursoEnVerano(Estudiante,Materia,Anio)).
	
cursoEnVerano(Estudiante,Materia,Anio):-
	fechaDeCursada(Estudiante,Materia,verano(_,_),Anio).

anioDeCursada(Estudiante,Anio):-
	curso(Estudiante,Materia),
	fechaDeCursada(Estudiante,Materia,_,Anio).
	
%5 Parte2----------------------------------
%tieneUnUnicoPerfil(Estudiante):-
%	esEstudiante(Estudiante),
%	perfil(sinDescanso(Estudiante).
	%forall(perfil(Perfiles),perfil(sinDescanso(Estudiante))).
	
%7 Parte2-----------------------------------------
%fechaDeCursada(Estudiante,Materia,Cuatrimestre,Anio).
%fechaDeCursada(Estudiante,Materia,anual,Anio).
%fechaDeCursada(Estudiante,Materia,verano(Mes,AnioCalendario),Anio).
fechaDeCursada(pepo,electricidadYMagnetismo,primerCuatrimestre,2012).
fechaDeCursada(pepo,matematicaI,primerCuatrimestre,2012).
fechaDeCursada(pepo,laboratorioDeComputacionI,primerCuatrimestre,2012).
fechaDeCursada(pepo,laboratorioDeComputacionII,segundoCuatrimestre,2012).
fechaDeCursada(pepo,matematicaIII,anual,2013).
%9 Parte2------------------------------------------
fechaDeCursada(lescano,matematicaI,primerCuatrimestre,2013).
fechaDeCursada(lescano,laboratorioDeComputacionI,segundoCuatrimestre,2013).
fechaDeCursada(lescano,electricidadYMagnetismo,verano(febrero,2014),2013).
%UsuarioTest Parte2------------------------------------------
fechaDeCursada(mas,laboratorioDeComputacionI,primerCuatrimestre,2013).
fechaDeCursada(mas,laboratorioDeComputacionI,segundoCuatrimestre,2013).
fechaDeCursada(mas,matematicaI,segundoCuatrimestre,2012).
fechaDeCursada(mas,matematicaI,primerCuatrimestre,2013).
fechaDeCursada(mas,matematicaII,anual,2013).	
fechaDeCursada(mas,matematicaII,anual,2014).
%fechaDeCursada(mas,matematicaII,anual,2012).	
%fechaDeCursada(mas,matematicaII,anual,2014).
fechaDeCursada(mas,electricidadYMagnetismo,verano(febrero,2014),2013).
fechaDeCursada(mas,electricidadYMagnetismo,verano(febrero,2015),2014).	

%TESTS
%6 Parte1----------------------------------
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

%8 Parte1y2----------------------------------
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

%7 Parte2----------------------------------	
	test(solo_puede_cursar_algoritmosI, nondet):-
		 puedeCursar(pepo,algoritmosI).

	test(solo_puede_cursar_sistemasOperativos, nondet):-
		 puedeCursar(pepo,sistemasOperativos).

	test(no_recurso_niguna_materia, fail):-
		materiasRecursadas(pepo,_).

	test(invictus, nondet) :-
		perfil(invictus(pepo)).

:- end_tests(pepo).

%10 Parte2---------------------------------
:- begin_tests(pablito_lescano).

	test(encaja_con_perfil_de_buenas_cursadas, nondet) :-
		perfil(buenasCursadas(lescano)).

	test(encaja_con_perfil_de_se_lo_que_hiciste_el_verano_pasado, nondet):-
		perfil(seLoQueHiciseElVeranoPasado(lescano)).

	test(invictus, nondet):-
	perfil(invictus(lescano)).

:- end_tests(pablito_lescano).