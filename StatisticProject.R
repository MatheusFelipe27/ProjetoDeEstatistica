#Guilherme Fernandes Xavier da Silva [gfxs]
#Matheus Felipe da Silva [mfs5]

#Questão 1

lista= read.csv('D:/Users/mfs5/Documents/ProjetoDeEstatistica/PlanilhaGOT - Página1.csv')
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
  episodios = c()
  notas = c()
  temporadas = c()
  aux=1;
  maior;
  menor;
  aux2=1;
  aux3=1;
  for (i in 1: length(x)){
    while(z[i]==aux2){
      if (aux3==1){
      maior=i;
      menor=i;
      }
      if (x[i]>x[maior]){
        maior=i;
      }
      if (x[i]<x[menor]){
        menor=i;
      }
      i=i+1;
      aux3=aux3+1;
    }
    notas = c(notas, x[menor], x[maior])
    episodios = c(episodios,y[menor], y[maior])
    temporadas = c(temporadas, z[menor], z[maior])
    aux2=aux2+1;
    aux3=1;
  }
  
  tabela = data.frame("TÍTULO"= episodios, "NOTA" = notas, "TEMPORADA"= temporadas);
  
  
  return (tabela);
}

x= comparar(lista[["Nota"]], lista[["Episodio"]], lista[["Temporada"]])
x