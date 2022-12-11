#zadanie domowe 1

#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

Vsr1 <- 120    #srednia predkosc z pierwszego odcinka w km/h
Vsr2 <- 90      #srednia predkosc z drugiego odcinka w km/h
#Vsr to całkowita droga przez całkowity czas czyli Vsr = (S+S)/(t1+t2)
#oba odcinki były równe wiec S=Vsr1*t1 i S=Vsr2*t2  => t1 = (Vsr2/Vsr1)*t2 i 2S = 2*Vsr2*t2
#podstawiając poniższe zalezności do gornego rowania: Vsr = (2*Vsr2*t2) / ((Vsr2/Vsr1)*t2 + t2)
#wyciągając t2 przed nawias w liczniku i mianowniki otrzymujemy ostateczny wzór

Vsr <- (2*Vsr2) / (Vsr2/Vsr1 +1)
# odpowiedz to 102.8571 km/h

#-------------------------------------------------------------------------------------------------------------------

#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.


df <-read.csv("dane.csv",header = TRUE, sep=";")
df <-df[rowSums(is.na(df)) != ncol(df),] #usuniecie ewntualnych wierszy z brakami danych
X <-as.list(df[['wzrost']]) #tablica reprezentująca wektor wzrost
Y <-as.list(df[['waga']]) #tablica reprezentująca wektor waga


dev_func <- function(arg_list) {
  suma <- 0
  srednia <-mean(unlist(arg_list))
  for (i in 1:length(arg_list)) {
    suma <- suma + (as.numeric(arg_list[i])-srednia)^2
  }
  return (sqrt(suma))
}


rXY <- function(list1, list2) {
  suma_cov <-0
  srednia1 <-mean(unlist(list1))
  srednia2 <-mean(unlist(list2))
  for (i in 1:length(list1)) {
    suma_cov <- suma_cov + ((as.numeric(list1[i])-srednia1)*(as.numeric(list2[i])-srednia2))
  }
  return (suma_cov/(dev_func(list1)*dev_func(list2)))
}

rXY(X,Y) # =0.9793459 / nim wartość bezwględna bliżej 1 tym większa zależność liniowa, w tym przypadku bardzo silna dodatnia zależność liniowa

#co potwierdza wykres
plot(df$wzrost, df$waga,xlab="Wzrost", ylab="waga", pch=19)

#-----------------------------------------------------------------------------------------------------------

#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

data_collection <- function(ile=1) {
  print('Wpisuj nazwy kolumn i zatwierdzaj Enter. Wciśnięie Enter bez wpiania nazwy oznacza zakończenie listy')
  lista_kol <-list()
  response <- "nieistotna_wartosc"
  while (response != "") {
    response <- readline("Podaj nazwe kolumny: ") 
    if (response != "") {lista_kol <- append(lista_kol, response)}
  }

  ramka = data.frame(matrix(nrow=ile, ncol=length(lista_kol),1)) #tworze df bo wypełnioną "1" (znam już liczbę kolumn i wierszy)
  names(ramka)<-c(lista_kol)                                      #podmieniam nazwy kolumn na zaczytane
  
  for (i in 1:ile) {                                              #w pętnli populujemy poszcegolne komórki, jeżei nie podmay wartosci zostanie "1"
    for (j in 1:length(lista_kol)) {
      response2 <- readline(paste("Wpisz Wartość dla kolumny",toString(lista_kol[j]),"linijka",toString(i),", :",sep=" "))
      if (response2 != "") {ramka[i,toString(lista_kol[j])] <- response2}
      }
  }
  show(ramka)
}    

#-----------------------------------------------------------------------------------------------------------

#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.

mojaf <- function(sciezka="D:\\PJATK\\R_ćw\\github_kody z zajęć\\smogKrakow2",nazwaKol="3_temperature",jakaFunkcja="mean",ilePlikow=1) {
  
  #sprawdzenie ile mamy plikow i sprawdzenie maksymalnej ilości
  lista_plikow <- list.files(sciezka, "*.csv")
  if (ilePlikow > length(lista_plikow)) {
    ilePlikow <- length(lista_plikow)
    print("nie ma tylu plikow w podanym katalogu, pod uwagę będą wzięte wszytskie")}

  #Utworzenie listy która finalnie będzie zawierać daną kolumne ze wszytskich plikow
  all_elements <- list()
  
  #Dodawanie do ww listy wartości z kolumn z pierwszych "ilePlikow" plików z pominięciem NA
  for (i in 1:ilePlikow){
    data <- read.csv(paste(sciezka,"\\",lista_plikow[i],sep=""), sep=",")
    seria <- as.list(data[[paste("X",nazwaKol,sep="")]])
    seria <- seria[!is.na(seria)]
    all_elements <- append(all_elements,seria)
  }  
  
  #Zwracanie wartości dla wybranej funkcji
  if (jakaFunkcja=="mean") {return (print(paste("Średnia to:",mean(unlist(all_elements)),sep=" ")))}
  if (jakaFunkcja=="median") {return (print(paste("Mediana to:",median(unlist(all_elements)),sep=" ")))}
  if (jakaFunkcja=="min") {return (print(paste("Minimalna to:",min(unlist(all_elements)),sep=" ")))}
  if (jakaFunkcja=="max") {return (print(paste("Maksymalna to:",max(unlist(all_elements)),sep=" ")))}
}

#Przyklady do wywolania:
#mojaf("D:\\PJATK\\R_ćw\\github_kody z zajęć\\smogKrakow2","140_humidity","max",9)

