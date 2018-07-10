library(tidyverse)


strikes<-new.env()
slopes<-new.env()
pef_v<-new.env()
prpef_v<-new.env()
opts_v<-new.env()#se guardan las opts con sus atributos correspondientes con un id de acuerdo al orden en que fueron creadas
factores<-new.env()
Refactores<-new.env()
Ropts_v<-new.env()#una vez que se calculan los factores para las fórmulas para cada rango el instrumento resultante se guarda aquí
i<-0
PE<-100
PRPE<-100

reset<-function(){
  
  assign('strikes',new.env(),envir=.GlobalEnv)
  assign('slopes',new.env(),envir=.GlobalEnv)
  assign('pef_v',new.env(),envir=.GlobalEnv)
  assign('prpef_v',new.env(),envir=.GlobalEnv)
  assign('opts_v',new.env(),envir=.GlobalEnv)
  assign('factores',new.env(),envir=.GlobalEnv)
  assign('Refactores',new.env(),envir=.GlobalEnv)
  assign('Ropts_v',new.env(),envir=.GlobalEnv)
  assign('i',0,envir=.GlobalEnv)

  }


#determina sí la llave 'a' existe en el ambiente 'b' 
not_in<-function(a,b){
  
  tryCatch(if(is.numeric(get(paste0(a),envir=b,inherits=FALSE))){return(FALSE)},error=function(e){return (TRUE)})
  
  }

#recupera el valor 'b' asociado a la llave 'a'
retrieve<-function(a,e){
  
  get(paste0(a),envir=e,inherits=FALSE)
  
}

#crea la opts de acuerdo a los parámetros desesados
add_opts<-function(strike=1,slope=1,pef=0,prpef=0,call=1){
  
  i<<-i+1
  maxfac<<-length(strikes)+length(slopes)+length(pef_v)+length(prpef_v)
  
  if(not_in(strike,strikes)){
    
    maxfac=maxfac+1
    f1=paste0("Factor ",maxfac)
    assign(paste0(strike),maxfac,strikes,inherits=FALSE)
    assign(paste0(eval(f1)),strike,factores,inherits=FALSE)
    
    
  }else{f1=paste0("Factor ",retrieve(strike,strikes))}
  
  if(not_in(slope,slopes)){
    
    maxfac=maxfac+1
    f2=paste0("Factor ",maxfac)
    assign(paste0(slope),maxfac,slopes,inherits=FALSE)
    assign(paste0(eval(f2)),slope,factores,inherits=FALSE)
  
    
  }else{f2=paste0("Factor ",retrieve(slope,slopes))}
  
  if(not_in(pef,pef_v)){
    
    maxfac=maxfac+1
    f3=paste0("Factor ",maxfac)
    assign(paste0(pef),maxfac,pef_v,inherits=FALSE)
    assign(paste0(eval(f3)),pef,factores,inherits=FALSE)

  }else{f3=paste0("Factor ",retrieve(pef,pef_v))}
  
  if(not_in(prpef,prpef_v)){
    
    maxfac=maxfac+1
    f4=paste0("Factor ",maxfac)
    assign(paste0(prpef),maxfac,prpef_v,inherits=FALSE)
    assign(paste0(eval(f4)),prpef,factores,inherits=FALSE)
   
  }else{f4=paste0("Factor ",retrieve(prpef,prpef_v))}
  
  opts<-list("strike"=strike,"slope"=slope,"pef"=pef,"prpef"=prpef,"f1"=f1,"f2"=f2,"f3"=f3,"f4"=f4,"call"=call)
  assign(paste0(i),opts,opts_v,inherits=FALSE)
  
  return(c(f1,f2,f3,f4))
  
}

#### La siguiente función evaluúa la fórmula general de pago en los puntos de inflexión (strike points)

patm<-function(i,VE){
  
  c=retrieve(i,Ropts_v)

    (VE-(PE*c$"strike"))*c$"slope"+(PE*c$"pef")+(PRPE*c$"prpef")
  
  
}

#crea el texto de los derechos de ejercicio para opciones americanas
crea_texto<-function(i,PE=get('PE',envir=.GlobalEnv,inherits=FALSE),PRPE=get('PRPE',envir=.GlobalEnv,inherits=FALSE)){
  
  c=retrieve(i,Ropts_v)
 
  
  # Tricotomía
  
  if(i==1){
    r1<-ifelse(patm(1,c$"strike")>=patm(2,c$"strike"),"menor o igual","menor")
    r2<-restructurados$factores[i,1]
    r3<-c('')
  }else if(i!=(restructurados$factores%>%nrow())) {
    r1<-ifelse(patm(i-1,retrieve(i-1,Ropts_v)$"strike")<=patm(i,retrieve(i-1,Ropts_v)$"strike"),"mayor o igual","mayor")
    r2<-restructurados$factores[i-1,1]
    r3<-ifelse(patm(i+1,c$"strike")<=patm(i,c$"strike"),"menor o igual","menor")
    r3<-paste0('y ',r3,' al Precio de Ejercicio multiplicado por el Factor ',restructurados$factores[i,1])
    
    
  }else{
    r1<-ifelse(patm(i,c$"strike")>=patm(i-1,c$"strike"),"mayor o igual","mayor")
    r2<-restructurados$factores[i,1]
    r3<-c('')
  }
  
  #Definimos cuantos sumandos tiene la fórmula
  m<-restructurados$factores[i,]
  
  if(restructurados$valores[i,2]==0){
    
    formula<-paste0("(PE x Factor ",m[3],") + (PRPE x Factor ",m[4],")")
    
  }else{
    
    formula<-paste0("(VE - (PE x Factor ",ifelse(i==1,m[1],restructurados$factores[i-1,1])," )) x Factor ",m[2],") + (PE x Factor ",m[3],") + (PRPE x Factor ",m[4],")")
    
  }
  
  
  paste(i,".Si el Valor de Referencia de Ejercicio del Título Opcional, en la Fecha de Ejercicio es ",r1," al Precio de Ejercicio multiplicado por el Factor",r2,r3," , la Emisora pagará:",
        formula,sep=" ")%>%gsub("\\s+", " ",.)
  
}


#función de pago

pago<-function(i=1,PE=get('PE',envir=.GlobalEnv,inherits=FALSE),PRPE=get('PRPE',envir=.GlobalEnv,inherits=FALSE),minimo=-1,maximo=1){
  
  c=retrieve(i,opts_v)
  
  #sirve para determinar si el pago corresponde a call o a un put
  
  if(c$"call"==1){f=1}else{f=-1}
  
  
  if(!(exists('df_pagos',envir=.GlobalEnv) && is.data.frame(get('df_pagos',envir=.GlobalEnv)))){
    
    assign('df_pagos',
            data_frame(rendimiento=seq(minimo,maximo, by = .01)*100)%>%
             mutate(VE=(1+(rendimiento/100))*100)%>%
             mutate(pago=ifelse((VE-(PE*c$"strike"))*f>=0,((VE-(PE*c$"strike"))*c$"slope")+(PE*c$"pef")+(PRPE*c$"prpef"),0)) ,
             envir=.GlobalEnv
            )
  }else{
    
    df_pagos_copia<-df_pagos
    df_pagos_copia$'pago'<-df_pagos$'pago'+ifelse((df_pagos$'VE'-(PE*c$"strike"))*f>=0,((df_pagos$'VE'-(PE*c$"strike"))*c$"slope")+(PE*c$"pef")+(PRPE*c$"prpef"),0)
    assign('df_pagos', df_pagos_copia,envir=.GlobalEnv)
    
  }
  
  
}



#genera la tabla de escenarios

tabla_grafico<-function(i=-1,j=1){
  
  rm('df_pagos',envir=.GlobalEnv)
  
  ifelse(length(opts_v)==0,print("No hay opts definidas"),lapply(names(opts_v),FUN='pago',minimo=i,maximo=j))
  
  df_pagos%>%ggplot(aes(x=VE,y=pago))+
    geom_line(colour="blue",size=1)+
    #geom_bar(aes(x=VE,y=ifelse(pago<0,0,pago)),stat="identity", colour="green")
    labs(y = "Derechos de los TEnedores MXN", x = "Valor de Referencia de Ejecución/Observación",size=5)+
    theme_bw()
  }


#genera el texto de  todos los intervalos
texto_pago<-function(){
  
  
  ifelse(length(Ropts_v)==0,print("No hay opts definidas"),textos<-lapply( as.integer(names(Ropts_v)),FUN='crea_texto'))

  textos[[max(as.integer(names(Ropts_v)))+1]]<-'Donde: VE = Valor de Referencia de Ejercicio PE = Precio de Ejercicio PRPE = Porcentaje Retornable de la Prima de Emisión'
  
  textos[["Factores"]]<- data_frame(Factores=names(factores))%>%
                         rowwise()%>%
                         mutate(Valores=retrieve(Factores,factores))%>%
                         knitr::kable()
    
  assign('textos',textos,envir=.GlobalEnv)
  
  return(textos)
  
}


#####Para corregir el factor 3 cuando las pendientes de opciones se anulan entre sí en ciertos rangos
#####La utilizaremos dentro la funciónrestructurar

#regresa una lista con el pago de cada instrumento activo en el rango sobre 100

pago_x_intervalo<-function(i=1,j,PE=get('PE',envir=.GlobalEnv,inherits=FALSE),PRPE=get('PRPE',envir=.GlobalEnv,inherits=FALSE)){
  
                  intervalos<-sort(as.numeric(names(strikes)))
                  
                  c=retrieve(i,opts_v)
                  
                  #sirve para determinar si el pago corresponde a call o a un put
                  
                  if(c$"call"==1){
                    
                    f=1
                    VE<-ifelse(j==length(intervalos),intervalos[length(intervalos)]+f,(intervalos[j]+intervalos[j+1])*.5)*100
                    
                  }else{
                    
                    f=-1
                    VE<-ifelse(j==1,intervalos[1]+f,(intervalos[j]+intervalos[j-1])*.5)*100
                    
                  }
                  
                  
                  payoff<-ifelse((VE-(PE*c$"strike"))*f>=0,((VE-(PE*c$"strike"))*c$"slope")+(PE*c$"pef")+(PRPE*c$"prpef"),0)
                  
                  return(payoff/100)
  
}

##utilizamos la función anterior para calcular el valor que se le debe sumar a la columna 3 de la tabla de factores

ajustes_pef<-function(){
            
           intervalos<-sort(as.numeric(names(strikes)))
            
            ajustes_X_c<-lapply(intervalos,function(x){
              
              activados_calls<-(lapply(opts_v,"[[",1)<=x )&(lapply(opts_v,"[[",9)==1)#&(lapply(opts_v,"[[",2)!=0)
              c<-lapply(as.integer(names(opts_v))[activados_calls],FUN='pago_x_intervalo',j=match(x,intervalos))%>%unlist()%>%sum()
              return(c)
              
            })%>%unlist()
            
            ajustes_X_p<-lapply(intervalos,function(x){
              
              activados_puts<-(lapply(opts_v,"[[",1)>=x )&(lapply(opts_v,"[[",9)!=1)#&(lapply(opts_v,"[[",2)!=0)
              c<-lapply(as.integer(names(opts_v))[activados_puts],FUN='pago_x_intervalo',j=match(x,intervalos))%>%unlist()%>%sum()
              
              return(c)
              
            })%>%unlist()
            
            ajustes_factor_3<-c(0,ajustes_X_c)+c(ajustes_X_p,0)
            
            return(ajustes_factor_3)

}


#####





restructurar<-function(){
  
                        #reordenamos los strikes
                        intervalos<-sort((names(strikes)))
                        
                        fac_calls<-lapply(intervalos,function(x){
                          
                          f1<-as.numeric(x)
                           
                          #aquellas opciones por arriba del precio de ejercicio x
                          activados<-(lapply(opts_v,"[[",1)<=x )&(lapply(opts_v,"[[",9)==1)
                          
                          if(sum(activados)==0){f2=f3=f4=0}else{
                          
                          f2<-lapply(opts_v,"[[",2)[activados]%>%reduce(`+`)%>%round(digits=4)
                          f3<-lapply(opts_v,"[[",3)[activados]%>%reduce(`+`)%>%round(digits=4)
                          f4<-lapply(opts_v,"[[",4)[activados]%>%reduce(`+`)%>%round(digits=4)
                          
                          }
                          
                          return(c(f1,f2,f3,f4))
                          
                        })
                        
                        fac_puts<-lapply(intervalos,function(x){
                          
                          f1<-as.numeric(x)
                          
                          #aquellas opciones por arriba del precio de ejercicio x
                          activados<-(lapply(opts_v,"[[",1)>=x )&(lapply(opts_v,"[[",9)!=1)
                          
                          if(sum(activados)==0){f2=f3=f4=0}else{
                         
                          f2<-lapply(opts_v,"[[",2)[activados]%>%reduce(`+`)%>%round(digits=4)
                          f3<-lapply(opts_v,"[[",3)[activados]%>%reduce(`+`)%>%round(digits=4)
                          f4<-lapply(opts_v,"[[",4)[activados]%>%reduce(`+`)%>%round(digits=4)
                          
                          }
                          
                          return(c(f1,f2,f3,f4))
                          
                        })
                        
                        fac_calls<- c(c(0,0,0,0),fac_calls%>%unlist())
                        fac_puts<-c(fac_puts%>%unlist(),c(0,0,0,0))
                        f_calls_puts<-rep(0,length(fac_puts))
                        indices<-(1:length(fac_puts)%%4!=1)
                        f_calls_puts[which(indices)]<-fac_calls[which(indices)]+fac_puts[which(indices)]
                        f_calls_puts[which(!indices)]<-pmax(fac_calls[which(!indices)],fac_puts[which(!indices)])
                        
                        #### Aquí llamo al método para corregir los factores
                        
                        ajustes_factor_3<-ajustes_pef() #son los sumando por escenario
                        ajustes_factor_3<-(f_calls_puts[which(1:length(f_calls_puts)%%4==2)]==0)*ajustes_factor_3 #solamente se asgina sí la pendiente en cada escenario es cero
                        ajustes_factor_3<-(f_calls_puts[which(1:length(f_calls_puts)%%4==3)]*!(f_calls_puts[which(1:length(f_calls_puts)%%4==2)]==0))+ajustes_factor_3#si la pendiente no es cero se toma el valor de la columna 3 de la matriz de refactores
                        f_calls_puts[which(1:length(f_calls_puts)%%4==3)]<-(ajustes_factor_3%>%round(digits=4)) #finalmente hacemos el ajuste
                        #restamos los factores 4 a los factores 3 para evitar contabilizarlos doblemente, ya que la función ajustes concentra el pago de un escalón en el factor 3
                        f_calls_puts[which(1:length(f_calls_puts)%%4==3)]<-f_calls_puts[which(1:length(f_calls_puts)%%4==3)]-(f_calls_puts[which(1:length(f_calls_puts)%%4==2)]==0)*f_calls_puts[which(1:length(f_calls_puts)%%4==0)]
                        
                        #### si las dos primeras entradas del arreglo anterior son cero se está sumando una constante siempre
                        #### para no incluirla el primer escenario que es cuando el subyacente está por debajo de cero quitamos las primeras cuatro entradas
                       
                        
                        if((f_calls_puts[1]==0)%>%reduce(`&`)){
                          
                          f_calls_puts<-f_calls_puts[-(1:4)]
                          
                        }
                        
                        
                        
                        ###
                        maximo<-0
                        v <- vector(mode="numeric", length=0)
                        
                        #### este lapply crea un vector v que determina la etiqueta del factor asociado
                        
                        lapply(1:length(f_calls_puts),function(x){
                          
                          if(x==1){maximo<<-maximo+1
                          v<<-c(v,maximo)
                          }else if(!(f_calls_puts[x]%in%f_calls_puts[1:(x-1)])){maximo<<-maximo+1
                          v<<-c(v,maximo)
                          }else if(!(f_calls_puts[x]%in%f_calls_puts[which(1:(x-1)%%4==x%%4)])){maximo<<-maximo+1
                          v<<-c(v,maximo)
                          }else{v<<-c(v,v[(match(f_calls_puts[x],f_calls_puts[which(1:(x-1)%%4==x%%4)])-1)*4+(x+3)%%4+1])}
                        
                        })
                        ####si los valores del primer intrumentos en 'v' son (0,0,0,0) lo podemos interpretar omo un bono que paga cero por lo que lo quitamos  de las tablas resúmen
                        
                        #if((v[1:4]==c(0,0,0,0))%>%reduce(`&`)){
                        #  
                        #      v[1:4]<-v[-(1:4)]
                        #      k<-5
                        #}else{k<-1}
                        
                        #### se agregan
                        
                        sapply(1:length(f_calls_puts),function(x){
                          
                          if(not_in(v[x],Refactores)){
                             
                            assign(paste0("Factor ",v[x]),f_calls_puts[x],Refactores,inherits=FALSE)
                          }  
                        })
                        
                        valores_x_segmento<-f_calls_puts%>%matrix(4)%>%t #matriz de valores del factores
                        fac_x_segmento<-v%>%matrix(4)%>%t #matriz de etiquetas del factor
                        
                       
                         tabla<-data_frame(Factores=names(Refactores))%>%
                           rowwise()%>%
                           mutate(id=as.integer(unlist(strsplit(Factores,' '))[2]))%>%
                           arrange(id)%>%
                           rowwise()%>%
                           mutate(Valores=retrieve(Factores,Refactores))%>%
                           select(Factores,Valores)#%>%
                           #knitr::kable()
                         
                         restructurados<-list(fac_x_segmento,valores_x_segmento,tabla)
                         names(restructurados)<-c("factores","valores","tabla")
                         assign('restructurados', restructurados,envir=.GlobalEnv)
                         
                         lapply(1:nrow(restructurados$valores),
                                function(i){
                                  ropts<-list("strike"=restructurados$valores[i,1],"slope"=restructurados$valores[i,2],"pef"=restructurados$valores[i,3],"prpef"=restructurados$valores[i,4])
                                  assign(paste0(i),ropts,Ropts_v,inherits=FALSE)
                                  }
                                )
                         
  
                         return(c(restructurados))#,list(ajustes_factor_3)))
                        
}






#DICI Escalera WC Europea_2
reset()
add_opts(1,0,0.085,1)
add_opts(1.05,0,-0.01,0)
add_opts(1.07,0,-0.025,0)
add_opts(1.1,0,-0.05,-1)
add_opts(.95,-1,0,0,0) #una put a 95
tabla_grafico()
restructurar()

#DICI 459
reset()
add_opts(1.025,1,0.025,1)
add_opts(1.15,-1,-0.025,-1)
add_opts(1.15,0,-0.125)
add_opts(1.15,0,0.15,1)
add_opts(1.025,0,0.025,1,0)
tabla_grafico()
restructurar()

reset()
add_opts(1.025,1)
add_opts(1.15,-1,.1)
tabla_grafico()
restructurar()


reset()
add_opts(1.1,.5,0,0)
add_opts(1.2,-.5,0,0,0)
tabla_grafico(i=-.1,j=1)
restructurar()
texto_pago()

reset()
add_opts(1,1,0,0)
add_opts(1.2,-1,0,0)
add_opts(0,0,1,0)
tabla_grafico()
restructurar()

#DICI_Serie_321 construido con calls y opciones binarias
reset()
add_opts(0,0,0,1)
add_opts(0,0,-1,0)
add_opts(.75,1,0,0)
add_opts(.8,-1,0,0)
add_opts(.8,0,.01,0)
add_opts(.9,0,0.0131,0)
add_opts(1.1,0,-.0131,0)
add_opts(1.2,0,-.02,0)
add_opts(1.2,-4/5,0,0)
add_opts(1.25,4/5,0,0)
tabla_grafico()
restructurar()

reset()
add_opts(1,1,0,0)
add_opts(1.2,-1,.1,0)
tabla_grafico()
restructurar()


#put spread P(k1)-P(k2)
reset()
add_opts(1,-1,0,0,0)
add_opts(1.2,1,0,0,0)
#add_opts(1.2,0,0.1,0,0)
tabla_grafico()
restructurar()

reset()
add_opts(1,0,.1,0,0)
tabla_grafico()
restructurar()

reset()
add_opts(1,0,.1,0)
tabla_grafico()
restructurar()


reset()
add_opts(1.2,1,.5,0)
add_opts(1.1,-1,.5,0,0)
add_opts(1.2,1,0.5,0)
add_opts(1.1,0,.5,0,0)
tabla_grafico(-.3,.3)
restructurar()



df_pagos%>%ggplot(aes(x=VE,y=pago))+
   geom_bar(aes(x=VE,y=ifelse(pago<0,0,pago)),stat="identity", position=position_dodge(), colour="#99CC33",fill="#99CC33")+
   geom_line(colour="blue",size=1)+
   geom_line(aes(x=VE,y=ifelse(pago*12<0,0,pago*12)),colour="#FFCC00", linetype="dashed", size=1)+
   scale_y_continuous(sec.axis = sec_axis(~.+get('PE',envir=.GlobalEnv,inherits=FALSE), name = "Derechos en las Fechas  de Ejercicio Tenedores en MXN",breaks=waiver()),breaks=waiver())+
   #scale_x_continuous(labels =waiver()*TRUE)+
   labs(y = "Derechos en las Fechas de Observación MXN", x = "Valor de Referencia de Ejecución/Observación",colour="#3399FF",size=5)+
   theme_bw()


#prueba=1


reset()
add_opts(1,1,0,0)
add_opts(1.1,-1,0,-1)
add_opts(1.1,0,-1,1)
add_opts(1.1,0,.5,1)
tabla_grafico()
restructurar()

#Prueba Jafet

reset()
add_opts(0,0,1,0)
add_opts(1,1,0,0)
add_opts(1.13,-1,0,0)
add_opts(1.13,0,-0.09,0)
tabla_grafico()
restructurar()
texto_pago()




#CALL_KO_CON_REBATE


reset()
add_opts(0,0,0,1)
add_opts(1,1,0,0)
add_opts(1.1,-1,0,0)
add_opts(1.1,0,-1,0)
add_opts(1.1,0,.5,0)
tabla_grafico()
restructurar()

#KIKO

reset()
add_opts(0,0,1,0)
add_opts(1,1,0,0)
add_opts(1.1,-1,0,0)
add_opts(1,-1,0,0,0)#put
add_opts(.8,1,0,0,0)#put
add_opts(.8,0,-.4,0,0)#put
add_opts(.8,1,0,0,0)#put
tabla_grafico()
restructurar()
texto_pago()


reset()
add_opts(1,1,0,0,0)
tabla_grafico()
restructurar()
texto_pago()

