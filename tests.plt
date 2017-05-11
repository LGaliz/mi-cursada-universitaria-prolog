:- begin_tests(materias_pesadas).

	test(algoritmosI_es_materia_Pesada):-
	esPesada(materia(algoritmosI,160)).
	
	test(basesDeDatos_es_materia_Pesada):-
	esPesada(materia(basesDeDatos,128)).
	
	test(metodosNumericos_NO_es_materia_Pesada,fail):-
	esPesada(materia(metodosNumericos,80)).
		
:- end_tests(materias_pesadas).