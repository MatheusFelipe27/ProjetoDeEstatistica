#Guilherme Fernandes Xavier da Silva [gfxs]
#Matheus Felipe da Silva [mfs5]

#Quest�o 1

lista= read.csv('D:/Users/mfs5/Documents/ProjetoDeEstatistica/PlanilhaGOT - P�gina1.csv')
print(lista)

#...........................................................................................................

#Quest�o 2

#Calcula-se a media, o desvio padr�o e a moda das notas dos epis�dios.

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




#...........................................................................................................

#Quest�o 3

#Calcula-se a media, o desvio padr�o e a mediana da audiencia dos epis�dios.

mediaAudiencia = mean(lista[["Audiencia.Em.milhoes."]])
mediaAudiencia


desvioAudiencia = sd(lista[["Audiencia.Em.milhoes."]])
desvioAudiencia


medianaAudiencia = median(lista[["Audiencia.Em.milhoes."]])
medianaAudiencia



#...........................................................................................................


#Quest�o 4

#Olha-se as notas e imprime-se epis�dios com a nota maior que 9

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


#Quest�o 5

#Retorna o nome dos epis�dios com maior e menor nota, e depois faz um dataframe com cada epis�dio encontrado

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
  
  tabela = data.frame("T�TULO"= episodios, "NOTA" = notas, "TEMPORADA"= temporadas);
  
  
  return (tabela);
}

x= comparar(lista[["Nota"]], lista[["Episodio"]], lista[["Temporada"]])
x


#...........................................................................................................

#Quest�o 6

#Calcula os desvios padr�o de todas a temporadas e imprime a que possui o menor

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

#Quest�o 7

#Faz uma m�dia dos epis�dios em que Brienne of Tarth participa

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

#Quest�o 8

#Imprime os personagens que aparecem em apenas um epis�dio na quarta temporada

onceCharacters = function(x){
  names = c()
  
  for(w in 1:length(x)){
    names = c(names, unlist(strsplit(toString(x[w]),",")))
  }
  characters = c()
  for(k in 1:length(names)){
    i = 0
    for(j in 1:length(names)){
      if(names[k]==names[j]){
        i = i + 1
      }
    }
    if (i == 1) {
      characters = c(characters, names[k])
    }
  }
  
  return(characters)
  
}

x = onceCharacters(lista[lista$Temporada==4,"Personagens"])
print(x)

#...........................................................................................................

#Quest�o 9

#Fun��o que dado o nome de um personagem, cria um histograma onde mostra a frequ�ncia de apari��o desse personagem a cada temporada.
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
  
Histo = hist(temporada, main = "Frequencia de Apari��o", right=FALSE, breaks = c(0,1,2,3,4,5,6,7,8), xlab = "Temporada",xlim = c(0,8), ylab = "Ocorrencia", ylim= c(0,10), col = "pink")
return (Histo);
}

x=histograma(lista[["Temporada"]], lista[["Personagens"]], "Brienne of Tarth(Gwendoline Christie)")


 
