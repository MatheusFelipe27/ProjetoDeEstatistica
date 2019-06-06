#Guilherme Fernandes Xavier da Silva [gfxs]
#Matheus Felipe da Silva [mfs5]

#Questão 1

lista= read.csv('C:/Users/mfs5/Documents/ProjetoDeEstatistica/PlanilhaGOT - Página1.csv')
print(lista)

#...........................................................................................................

#Questão 2

mediaNota = mean(lista[["Nota"]])
mediaNota

desvioNota = sd(lista[["Nota"]])
desvioNota


modaNota =function(x){
  sort(x)
  
  a = length(x)-1
  aux=1;
  aux2=1;
  aux3=x[1];
  for (i in 1:a){
    while(x[i]==x[i+1]){
      aux=aux+1;
      i=i+1;
    }
    if(aux>aux2){
      aux2=aux;
      aux3=x[i];
    }
    aux=1;
  }
  return (aux3);
}
x = modaNota(lista[["Nota"]])
x


#Calcula-se a media, o desvio padrão e a moda das notas dos episódios.


#...........................................................................................................

#Questão 3

mediaAudiencia = mean(lista[["Audiencia.Em.milhoes."]])
mediaAudiencia


desvioAudiencia = sd(lista[["Audiencia.Em.milhoes."]])
desvioAudiencia


medianaAudiencia = median(lista[["Audiencia.Em.milhoes."]])
medianaAudiencia

#Calcula-se a media, o desvio padrão e a mediana da audiencia dos episódios.


#...........................................................................................................


#Questão 4

notasmaioresque9 = function(x, y){
  episodios = c()
  for (i in 1:length(x)){
    if (x[i]>=9){
      episodios= c(episodios,toString(y[i]));
    }
  }
  return (episodios);
}
x=notasmaioresque9(lista[["Nota"]], lista[["Episodio"]])
x





#...........................................................................................................


#Questão 5

comparar = function(x,y,z){
  notas = c()
  episodios = c()
  temporadas = c()
  aux=1;
  maior=0;
  menor=0;
  aux2=1;
  aux3=1;
  count =1;
  while (count<=length(x)){
    while(count <= length(x) && z[count]==aux2){
      if (aux3==1){
        maior=count;
        menor=count;
      }
      else {
        if (x[count]>x[maior]){
          maior=count;
        }
        if (x[count]<x[menor]){
          menor=count;
        }
      }
      count=count+1;
      aux3=aux3+1;
    }
    notas = c(notas, x[menor], x[maior])
    episodios = c(episodios,toString(y[menor]), toString(y[maior]))
    temporadas = c(temporadas, z[menor], z[maior])
    aux2=aux2+1;
    aux3=1;
  }
  
  tabela = data.frame("TÍTULO"= episodios, "NOTA" = notas, "TEMPORADA"= temporadas);
  
  
  return (tabela);
}

x= comparar(lista[["Nota"]], lista[["Episodio"]], lista[["Temporada"]])
x


#...........................................................................................................

#Questão 6

menorDesvio = function(x){
  temporada = c();
  menorDesvio = 0;
  temporada = 0;
  aux = 0;
  for(i in 1:8){
    aux = sd(x[x$Temporada==i,"Audiencia.Em.milhoes."])
    if(menorDesvio==0 || aux<menorDesvio){
      menorDesvio = aux;
      temporada = i;
    }
  }
  return(temporada)
}
x = menorDesvio(lista)
x

#...........................................................................................................

#Questão 7

brienneappears= function(n,e,p){
  episodios = c()
  notas = c()
  for (i in 1: length(n)){
    v = unlist(strsplit(toString(p[i]),","));
    for (j in v){
      if (j=="Brienne of Tarth(Gwendoline Christie)"){
        episodios = c(episodios, j)
        notas = c(notas, n[i])
      }
    }
  }
  
  media = mean(notas)
  return (media);
}

x = brienneappears(lista[["Nota"]], lista[["Episodio"]], lista[["Personagens"]])
x

#...........................................................................................................


#Questão 9

histograma = function(t,p,namep){
  temporada = c()
  for (i in 1: length(p)){
    v = unlist(strsplit(toString(p[i]),","));
    for (j in v){
      if (j ==namep){
        temporada = c(temporada, t[i]-1)
      }
    }
  }
  
Histo = hist(temporada, main = "Frequencia de Aparição", right=FALSE, breaks = c(0,1,2,3,4,5,6,7,8), xlab = "Temporada",xlim = c(0,8), ylab = "Ocorrencia", ylim= c(0,10), col = "pink")
return (Histo);
}

x=histograma(lista[["Temporada"]], lista[["Personagens"]], "Brienne of Tarth(Gwendoline Christie)")


 
