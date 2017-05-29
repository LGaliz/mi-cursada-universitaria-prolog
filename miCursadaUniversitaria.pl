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
	notaCursada(Estudiante, Materia, Nota, _),
	notaAprobacion(Nota).

promociono(Estudiante,Materia) :-
	promocionable(Materia),
	notaCursada(Estudiante, Materia, Nota,_),
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
	notaCursada(Estudiante,_,_,_).

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

notaCursada(pepo,Materia,8,cuatrimestral(1,2012)) :- materiaInicial(Materia).
notaCursada(pepo,laboratorioDeComputacionII, 5, cuatrimestral(2,2012)).
notaCursada(pepo,matematicaII, 6, cuatrimestral(2,2012)).
notaCursada(pepo,matematicaIII, 4, anual(2013)).

%9 Parte2----------------------------------
notaCursada(lescano,electricidadYMagnetismo,9,verano(febrero,2014)).
notaCursada(lescano,matematicaI,8,cuatrimestral(1,2013)).
notaCursada(lescano,laboratorioDeComputacionI,10,cuatrimestral(2,2013)).

%UsuarioTest Parte2------------------------
notaCursada(mas,laboratorioDeComputacionI,3,cuatrimestral(1,2013)).
notaCursada(mas,laboratorioDeComputacionI,4,cuatrimestral(2,2013)).
notaCursada(mas,matematicaI,2,cuatrimestral(1,2013)).
notaCursada(mas,matematicaI,6,cuatrimestral(2,2013)).
notaCursada(mas,matematicaII,2,anual(_,2013)).
notaCursada(mas,matematicaII,6,anual(_,2014)).
notaCursada(mas,electricidadYMagnetismo,9,verano(febrero,2014)).
notaCursada(mas,electricidadYMagnetismo,9,verano(febrero,2015)).
notaCursada(mas,matematicaII,10,anual(_,2014)).
notaCursada(mas,matematicaII,10,anual(_,2014)).

%1 Parte2----------------------------------
satisfaceRegimenCorrelativas(Estudiante,Materia) :-
	cursoTodasLasMateriasNecesarias(Estudiante,Materia),
	aproboElSegundoNivelDeCorrelativas(Estudiante,Materia).

puedeCursar(Estudiante,Materia):-
	esMateria(materia(Materia,_)),
	not(curso(Estudiante,Materia)),
	satisfaceRegimenCorrelativas(Estudiante,Materia).

cursoTodasLasMateriasNecesarias(Estudiante,Materia):-
	forall(sonNecesariasParaCursar(Materia,Correlativa), curso(Estudiante,Correlativa)).

aproboElSegundoNivelDeCorrelativas(Estudiante,Materia):-
	forall(esCorrelativaDeSegundoNivel(Materia, CorrelativaSegundoNivel), aprobo(Estudiante, CorrelativaSegundoNivel)).

esCorrelativaDeSegundoNivel(Materia,CorrelativaSegundoNivel):-
	esCorrelativaDe(Materia, CorrelativaPrimerNivel),
	esCorrelativaDe(CorrelativaPrimerNivel, CorrelativaSegundoNivel).

%2 Parte2------------------------------------------
enQueCuatrimestreCurso(Estudiante,Materia,Cuatrimestre,Anio):-
	notaCursada(Estudiante,Materia,_,cuatrimestral(Cuatrimestre,Anio)).

materiasRecursadas(Estudiante,Materia) :-
	notaCursada(Estudiante,Materia,_,Fecha1),
	notaCursada(Estudiante,Materia,_,Fecha2),
	Fecha1 \= Fecha2.

%4 Parte2------------------------------------------
esElSiguiente(Anio,OtroAnio):-
	Anio is OtroAnio - 1.

cursoInmediatamente(Estudiante,Materia) :-
	notaCursada(Estudiante,Materia,_,Fecha),
	notaCursada(Estudiante,Materia,_,Fecha2),
	sonInmediatas(Fecha,Fecha2).

sonInmediatas(anual(_,Anio1), anual(_,Anio2)) :- esElSiguiente(Anio2,Anio1).

sonInmediatas(anual(_,Anio1), cuatrimestral(Cuatrimestre2,Anio2)) :-
	esElSiguiente(Anio1,Anio2),
	primerCuatrimestre(Cuatrimestre2).

sonInmediatas(cuatrimestral(Cuatrimestre1,Anio1), cuatrimestral(Cuatrimestre2,Anio2)) :-
	segundoCuatrimestre(Cuatrimestre2),
	primerCuatrimestre(Cuatrimestre1),
	Anio1 == Anio2.

sonInmediatas(cuatrimestral(Cuatrimestre1,Anio1), cuatrimestral(Cuatrimestre2,Anio2)) :-
	segundoCuatrimestre(Cuatrimestre1),
	primerCuatrimestre(Cuatrimestre2),
	esElSiguiente(Anio2,Anio1).

sonInmediatas(verano(_,Anio1), verano(_,Anio2)) :-
	sonInmediatas(anual(_,Anio1), anual(_,Anio2)).

sonInmediatas(verano(_,Anio1), cuatrimestral(Cuatrimestre2,Anio2)) :-
	Anio2 == Anio1,
	primerCuatrimestre(Cuatrimestre2).

primerCuatrimestre(Cuatrimestre) :- 1 is Cuatrimestre.

segundoCuatrimestre(Cuatrimestre) :- 2 is Cuatrimestre.

perfil(sinDescanso,Estudiante):-
	materiasRecursadas(Estudiante,Materia),
	forall(materiasRecursadas(Estudiante,Materia), cursoInmediatamente(Estudiante,Materia)).

perfil(invictus,Estudiante):-
	esEstudiante(Estudiante),
	not(materiasRecursadas(Estudiante,_)).

perfil(repechaje,Estudiante):-
	materiasRecursadas(Estudiante,Materia),
	notaCursada(Estudiante,Materia,_,cuatrimestral(Cuatrimestre2,Anio2)),
	promociono(Estudiante,Materia),
	notaCursada(Estudiante,Materia,Nota,anual(_,Anio1)),
	not(notaAprobacion(Nota)),
	sonInmediatas(anual(_,Anio1), cuatrimestral(Cuatrimestre2,Anio2)).

perfil(buenasCursadas,Estudiante):-
	forall(materiasPromocionablesQueCurso(Estudiante,Materia), promociono(Estudiante,Materia)).

perfil(seLoQueHicisteElVeranoPasado,Estudiante):-
	forall(anioDeCursada(Estudiante,Anio),cursoEnVerano(Estudiante,_,Anio)).

materiasPromocionablesQueCurso(Estudiante,Materia) :-
	notaCursada(Estudiante,Materia,_,_),
	promocionable(Materia).

cursoEnVerano(Estudiante,_,Anio):-
	AnioLectivo is Anio + 1,
	notaCursada(Estudiante,_,_,verano(_,AnioLectivo)).

anioDeCursada(Estudiante,Anio):-
	notaCursada(Estudiante,_,_,Fecha),
	anio(Fecha, Anio).

anio(cuatrimestral(_,Anio),Anio).
anio(anual(_,Anio),Anio).
anio(verano(_,Anio), Anio2) :- Anio2 is Anio - 1.

%5 (BONUS) Parte2----------------------------------
tieneUnUnicoPerfil(Estudiante):-
	not(tieneMasDeUnPerfil(Estudiante)).

tieneMasDeUnPerfil(Estudiante) :-
	perfil(UnPerfil,Estudiante),
	perfil(OtroPerfil,Estudiante),
	UnPerfil \= OtroPerfil.

%6 Parte2----------------------------------

indiceDeDesempenioAcademico(Estudiante,Materia,Indice) :-
	notaCursada(Estudiante,Materia,Nota,cuatrimestral(Cuatrimestre,_)),
	Indice is Nota - Cuatrimestre.

indiceDeDesempenioAcademico(Estudiante,Materia,Indice) :-
	notaCursada(Estudiante,Materia,Nota,anual(_,_)),
	Indice is Nota.

indiceDeDesempenioAcademico(Estudiante,Materia,Indice) :-
	notaCursada(Estudiante,Materia,Nota,verano(Mes,Anio)),
	not(restriccionDeVerano(Mes,Anio)),
	Indice is Nota / 2.

indiceDeDesempenioAcademico(Estudiante,Materia,Indice) :-
	notaCursada(Estudiante,Materia,Nota,verano(Mes,Anio)),
	restriccionDeVerano(Mes,Anio),
	Indice is Nota.

restriccionDeVerano(Mes,AnioCalendario):-
	letrasDelMes(Mes,Cantidad),
	esPar(AnioCalendario + Cantidad).

esPar(Numero) :-
	0 is mod(Numero,2).

letrasDelMes(Mes,Cantidad):-
	atom_length(Mes,Cantidad).

%TESTS CASOS DE PRUEBA
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

%8 Parte1------------------------------------
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

%8 Parte2----------------------------------
:- begin_tests(pepo_parte2).

	test(solo_puede_cursar_algoritmosI_y_sistemas_operativos,
		 set(Materia == [algoritmosI,sistemasOperativos])):-
		 puedeCursar(pepo,Materia).

	test(no_recurso_niguna_materia, fail):-
		materiasRecursadas(pepo,_).

:- end_tests(pepo_parte2).
%9 Parte2----------------------------------

:- begin_tests(perfiles_pablito_lescano).

        test(encaja_con_perfil_de_buenas_cursadas_y_se_lo_que_hiciste_el_verano_pasado,
     	 set(Perfil == [buenasCursadas,seLoQueHicisteElVeranoPasado]), nondet):-
		perfil(Perfil,lescano).

:- end_tests(perfiles_pablito_lescano).

:- begin_tests(estudiantes_invictus).

        test(invictus, set(Estudiante == [pepo,lescano])):- perfil(invictus,Estudiante).

:- end_tests(estudiantes_invictus).

:- begin_tests(indices_de_valoracion).

	test(valoracion_electricidadYMagnetismo_es_4yMedio, nondet) :-
		indiceDeDesempenioAcademico(lescano,electricidadYMagnetismo,4.5).

        test(valoracion_laboII_pepo, nondet) :-
		indiceDeDesempenioAcademico(pepo,laboratorioDeComputacionII,3).

:- end_tests(indices_de_valoracion).
