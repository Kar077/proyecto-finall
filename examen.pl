

/*
INTERFAZ GRAFICA: Esta parte del sistema experto es la que se encarga de
interactuar con la persona comun, mostrar imagenes, botones, textos, etc.

INICIAR SISTEMA EXPERTO:
PARA CORRER EL PROGRAMA, ES NESESARIO TENER INSTALADO SWI PROLOG
EJECUTAR EL SISTEMA Y LUEGO SOLO CONSULTAR TODO, AUTOMATICAMENTE SE
ABRIRA LA VENTANA DEL PROGRAMA
*/
 :- use_module(library(pce)).
 :- pce_image_directory('./imagenes').
 :- use_module(library(pce_style_item)).
 :- dynamic color/2.

 resource(img_principal, image, image('img_principal.jpg')).
 resource(portada, image, image('portada.jpg')).

 resource(sarampion, image, image('trat_bullying.jpg')).
 resource(resfriado, image, image('trat_bullying2.jpg')).
 resource(influenza, image, image('trat_bullying3.jpg')).
 resource(hepatitisc, image, image('trat_bullying4.jpg')).
 resource(hepatitisb, image, image('trat_bullying5.jpg')).

 resource(diagnostico_desconocido, image, image('desconocido.jpg')).

%ExCLUSION SOCIAL
 resource(des_precio, image, image('des_precio.jpg')).
 resource(ex_clusion, image, image('ex_clusion.jpg')).
 resource(no_hablan, image, image('no_hablan.jpg')).
 resource(feo, image, image('feo.jpg')).
%HOSTIGAMIENTO
 resource(d, image, image('d.jpg')).
 resource(g, image, image('g.jpg')).
 resource(ich, image, image('ich.jpg')).
 resource(f, image, image('f.jpg')).
%AMENAZAS
 resource(ame_nazas, image, image('ame_nazas.jpg')).
 resource(alert, image, image('alert.jpg')).
 resource(e, image, image('e.jpg')).


%MANIPULACION E INTIMIDACION
 resource(a, image, image('a.jpg')).
 resource(b, image, image('b.jpg')).
 resource(i, image, image('i.jpg')).
 resource(h, image, image('h.jpg')).

%AGRESIONES
 resource(j, image, image('j.jpg')).
 resource(agresividad, image, image('agresividad.jpg')).
 resource(violencia, image, image('violencia.jpg')).
 resource(c, image, image('c.jpg')).

 mostrar_imagen(Pantalla, Imagen) :- new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(100,80)).
  mostrar_imagen_tratamiento(Pantalla, Imagen) :-new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(20,100)).
 nueva_imagen(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(0,0)).
  imagen_pregunta(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(500,60)).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

botones:-borrado,
                send(@boton, free),
                send(@btntratamiento,free),
                mostrar_diagnostico(Enfermedad),
                send(@texto, selection('El Diagnóstico a partir de los datos es:')),
                send(@resp1, selection(Enfermedad)),
                new(@boton, button('Iniciar consulta',
                message(@prolog, botones)
                )),

                new(@btntratamiento,button('Detalles y Tratamiento',
                message(@prolog, mostrar_tratamiento,Enfermedad)
                )),
                send(@main, display,@boton,point(20,450)),
                send(@main, display,@btntratamiento,point(138,450)).



  mostrar_tratamiento(X):-new(@tratam, dialog('Tratamiento')),
                          send(@tratam, append, label(nombre, 'Explicación: ')),
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

tratamiento(X):- send(@lblExp1,selection('De Acuerdo Al Diagnóstico El Tratamiento Es:')),
                 mostrar_imagen_tratamiento(@tratam,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


   preguntar(Preg,Resp):-new(Di,dialog('Colsultar Datos:')),
                        new(L2,label(texto,'Responde las siguientes preguntas')),
                        id_imagen_preg(Preg,Imagen),
                        imagen_pregunta(Di,Imagen),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% x%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
interfaz_principal:-new(D,dialog('Datos Personales')),
    new(T1,text_item('Nombres')),
    new(T2,text_item('Apellidos')),
    new(T3,text_item('Colegio')),

    new(S1,button(sgt,and(message(@prolog,interfazz_principal),message(D,destroy)))),

    send_list(D,append,[T1,T2,T3,S1]),
    send(D,open_centered).


  interfazz_principal:-new(@main,dialog('SISTEMA EXPERTO DIAGNOSTICADOR DE ENFERMEDADES',
        size(1000,1000))),
        new(@texto, label(nombre,'El Diagnóstico a partir de los datos es:',font('times','roman',18))),
        new(@resp1, label(nombre,'',font('times','roman',22))),
        new(@lblExp1, label(nombre,'',font('times','roman',14))),
        new(@lblExp2, label(nombre,'',font('times','roman',14))),
        new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
        new(@boton, button('Iniciar consulta',message(@prolog, botones))),


        new(@btntratamiento,button('Â¿Tratamiento?')),




        nueva_imagen(@main, img_principal),
        send(@main, display,@boton,point(138,450)),
        send(@main, display,@texto,point(20,130)),
        send(@main, display,@salir,point(300,450)),
        send(@main, display,@resp1,point(20,180)),
        send(@main,open_centered).

       borrado:- send(@resp1, selection('')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  crea_interfaz_inicio:- new(@interfaz,dialog('BIENVENIDO AL SISTEMA DIAGNOSTICADOR EXPERTO',
  size(1000,1000))),

  mostrar_imagen(@interfaz, portada),

  new(BotonComenzar,button('COMENZAR',and(message(@prolog,interfaz_principal) ,
  and(message(@interfaz,destroy),message(@interfaz,free)) ))),
  new(BotonSalir,button('SALIR',and(message(@interfaz,destroy),message(@interfaz,free)))),
  send(@interfaz,append(BotonComenzar)),
  send(@interfaz,append(BotonSalir)),
  send(@interfaz,open_centered).

  :-crea_interfaz_inicio.


/* BASE DE SISTEMA IDENTIFICADOR DE ENFERMEDADES.
*/
% SARAMPION
conocimiento('sarampion',
['tines tos', 'tienes fiebre',
'tines perdida del apetito','tines salpullido']).
% RESFRIADO
conocimiento('resfriado',
['tienes tos', 'tienes fiebre',
'tienes cuerpo cortado','tienes dolor de huesos']).
% influenza
conocimiento('influenza',
['tienes fiebre',
'tienes tos', 'tienes escurrimiento nasal']).
%hepatis tipo c
conocimiento('hepatitisc',
['tienes apariencia amarillenta',
'tienes fiebre',
'tienes diarrea',
'tienes nauseas']).
%  hepatisB
conocimiento('hepatitisb',
['tienes apariencia amarrillenta','tienes fiebre',
'tienes bomito','tienes dolor de estomago']).

% SARAMPION
id_imagen_preg('tienes tos ','feo').
id_imagen_preg('tienes fiebre','ex_clusion').
id_imagen_preg('tienes perdida del apetito','no_hablan').
id_imagen_preg('tienes salpullido','des_precio').

% RESFRIADO
id_imagen_preg('tienes tos','d').
id_imagen_preg('tienes fiebre','g').
id_imagen_preg('tienes dolor de huesos','ich').
id_imagen_preg('tienes cuerpo cortado','f').

% INFLUENZA
id_imagen_preg('tienes fiebre','ame_nazas').
id_imagen_preg('tienes tos','alert').
id_imagen_preg('tienes escurrimiento nasal','e').

% HEPATITISC
id_imagen_preg('tienes apariencia amarillenta','a').
id_imagen_preg('tienes fiebre','b').
id_imagen_preg('tienes diarrea','i').
id_imagen_preg('tienes nauseas','h').

% HEPATITISB
id_imagen_preg('tienes apariencia amarillenta','j').
id_imagen_preg('tienes fiebre','agresividad').
id_imagen_preg('tienes bomito','violencia').
id_imagen_preg('tienes dolor de estomago','c').

 /* MOTOR DE INFERENCIA: Esta parte del sistema experto se encarga de
 inferir cual es el diagnostico a partir de las preguntas realizadas
 */
:- dynamic conocido/1.

  mostrar_diagnostico(X):-haz_diagnostico(X),clean_scratchpad.
  mostrar_diagnostico(diagnostico_desconocido):-clean_scratchpad .

  haz_diagnostico(Diagnosis):-
                            obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas),
                            prueba_presencia_de(Diagnosis, ListaDeSintomas).


obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas):-
                            conocimiento(Diagnosis, ListaDeSintomas).


prueba_presencia_de(Diagnosis, []).
prueba_presencia_de(Diagnosis, [Head | Tail]):- prueba_verdad_de(Diagnosis, Head),
                                              prueba_presencia_de(Diagnosis, Tail).


prueba_verdad_de(Diagnosis, Sintoma):- conocido(Sintoma).
prueba_verdad_de(Diagnosis, Sintoma):- not(conocido(is_false(Sintoma))),
pregunta_sobre(Diagnosis, Sintoma, Reply), Reply = 'si'.


pregunta_sobre(Diagnosis, Sintoma, Reply):- preguntar(Sintoma,Respuesta),
                          process(Diagnosis, Sintoma, Respuesta, Reply).


process(Diagnosis, Sintoma, si, si):- asserta(conocido(Sintoma)).
process(Diagnosis, Sintoma, no, no):- asserta(conocido(is_false(Sintoma))).


clean_scratchpad:- retract(conocido(X)), fail.
clean_scratchpad.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).


