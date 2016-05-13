

**R dla humanistów — Warsztaty — LaCH — 10-11 V 2016 r. — Sven Sellmer**

Mam nadzieję, że następujące informacje wraz z własnymi notatkami umożliwią wejście do krainy R. W wypadku problemów i pytań, proszę śmiało napisać na adres: sven@amu.edu.pl.

ZMIENNE

    <- : przypisanie wartości

Najważniejsze podstawowe typy danych

    numeric (double)
    integer
    character: “”
    logical: T(RUE), F(ALSE): zarezerwowane nazwy

    a <- 1

    a <- "1" # character zawsze w cudzysłowiu!

    a <- TRUE

Po wpisaniu samej nazwy zmiennej na ekranie pojawia się jej wartość.

    a <- "Czołem"
    a

    ## [1] "Czołem"

Do zmiennej można stosować funkcję, np.

    a <- 9
    sqrt(a) # pierwiastek kwadratowy

    ## [1] 3

Ale samo stosowanie funkcji nie zmienia wartości zmiennej. Do tego jest potrzebny `<-`. Proszę zauważyć:

    a <- 9
    sqrt(a)

    ## [1] 3

    a

    ## [1] 9

    a <- sqrt(a)
    a

    ## [1] 3

Typ zmiennej można sprawdzać typ przy pomocy funkcji:

    typeof()

    a <- TRUE
    typeof(a)

    ## [1] "logical"

bardziej złożone rodzaje danych (jak `factor` czy `data frame`) przy pomocy:

    class()

    class(mtcars)

    ## [1] "data.frame"

Ale najlepiej korzystać z

    str()

która to funkcja podaje podstawowe informacje nt. wszystkich rodzajów danych.

    a <- "Czołem"
    str(a)

    ##  chr "Czołem"

Inne funkcje związane z typami zmiennych

    as.integer() itd.: zdefiniować zmienną jako int. itd.

    is.integer() itd.: sprawdzić, czy zm. jest int. itd.

    a <- as.character(1)
    a + 3 

    ## Error in a + 3: nicht-numerisches Argument für binären Operator

    is.integer(a) # Ojej, błąd! Dlaczego?? Sprawdźmy:

    ## [1] FALSE

    a <- as.integer(a)
    a + 3 # Teraz można:

    ## [1] 4

WEKTOR (`vector`): uporządkowany ciąg elementów tego samego typu

    c() – concatenate

    muza <- c("abba", "beatles", "clapton", "doda", "beatles", "abba", "abba", "clapton", "clapton", "abba", "łomot", "zappa") 

    `:` – wektor od … do

    1:10

    ##  [1]  1  2  3  4  5  6  7  8  9 10

    length() #długość, czyli liczba elementów wektora

Podstawowe informacje nt. wektorów liczbowych:

    mean()
    
    median()
    
    sum()

Wybieranie / filtrowanie:

   

     [ ]

    muza[3]

    ## [1] "clapton"

    muza[-3] # cały wektor bez trzeciego elementu
    
    wybieranie za pomocą wektorów logicznych:
    
    muza[muza == "abba"]
    
    ## [1] "abba" "abba" "abba" "abba"

Operatory logiczne (przykłady zastosowania zob. niżej):

    == : równa się (uwaga! 2 x =)

    != : nie równa się

    <, >, >= itp.

    &, | : logiczne i/lub

    %in% : wspólny zbiór dwóch wektorów

    muza[muza %in% c("clapton", "doda")]

    [1] "clapton" "doda"    "clapton" "clapton"

    grepl() # wybieranie za pomocą wyrażeń regularnych:

np. elementy ze zbitką spółgłoskową:

    muza[grepl("[^aeiou][^aeiou]", muza)]

     [1] "abba"    "beatles" "clapton" "beatles" "abba"    "abba"    "clapton"
     [8] "clapton" "abba"    "zappa"

albo wszystkie elementy niezawierające “a”:

    muza[!grepl("a", muza)]

    [1] "łomot"

Kolejność alfabetyczna (znaki) lub numeryczna (liczby):

    order() #uważać na locale, LC_COLLATE; ew. sprawdzić za pomocą: Sys.getlocale()

Przy okazji miałbym prośbę: czy Państwo, którzy nie mieli problemów z polskimi znakami, mogliby przesłać mi wyniki funkcji `Sys.getlocale()` + informację nt. systemu operacyjnego (Windows 7 lub 10 itp.)?

Można ustalić kolejność:

    order(muza)

    ##  [1]  1  6  7 10  2  5  3  8  9  4 11 12

… albo uporządkować w tej kolejności:

    muza[order(muza)]
    
    ##  [1] "abba"    "abba"    "abba"    "abba"    "beatles" "beatles" "clapton"
    ##  [8] "clapton" "clapton" "doda"    "łomot"   "zappa"

Indeksy danej wartości:

    which()

    which(muza == "beatles")

    ## [1] 2 5

Tabelka częstotliwości:

    table()

    table(muza)

    ## muza
    ##    abba beatles clapton    doda   łomot   zappa 
    ##       4       2       3       1       1       1

To wektor liczbowy, “abba” itd. jego nazwy/symbole (names), zob.

    names(table(muza))
    
    ## [1] "abba"    "beatles" "clapton" "doda"    "łomot"   "zappa"

ZŁOŻONE STRUKTURY DANYCH

    matrix: tylko jeden typ
    data frame: kolumny wektorami (wektory różnego typu możliwe)
    list: różne typy

**Data frames**

Najważniejsza struktura to data frame (od tego miejsca: “df”) przydatne funkcje do wstępnego zapoznania się z df:

    str()

    head()

    tail()

Wybiera się w ten sposób:

    [rząd, kolumna]

    [rząd, ] – wszystkie kol.

    mtcars[1:3, ]

    ##                mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    
        [ ,kol] # wszystkie rządy
    
    mtcars[ ,c(1,4)]

Kolumny można też wybrać według nazwy za pomocą $, np.:

    mtcars$mpg[3:5]

    ## [1] 22.8 21.4 18.7

Wybieranie za pomocą wektorów logicznych:

    mtcars[mtcars$hp > 150 & mtcars$wt < 3, ]
    
    ##               mpg cyl disp  hp drat   wt qsec vs am gear carb
    ## Ferrari Dino 19.7   6  145 175 3.62 2.77 15.5  0  1    5    6

(porządkowanie wg wartości danej kolumny:

    mtcars[order(mtcars$hp), ]

Odwrotna kolejność:

    mtcars[order(-mtcars$hp), ]

Dodać kolumny / wiersze:

    cbind()

    rbind() # można dodać w ten sposób cały df tego samego typu

Obliczyć wartości grupowo (wg. faktorów):

    tapply()

`tapply(mtcars$mpg, as.factor(mtcars$cyl), mean`)

    ##        4        6        8 
    ## 26.66364 19.74286 15.10000

**Listy**

Przykład

    nasza_lista <- list(c(1:3), c("a", "b", "c"), c(TRUE, FALSE, FALSE, FALSE))
    nasza_lista

    [[1]]
    [1] 1 2 3
    
    [[2]]
    [1] "a" "b" "c"
    
    [[3]]
    [1]  TRUE FALSE FALSE FALSE

wybiera się w ten sposób:

    nasza_lista[[2]][3]

    ## [1] "c"

FUNKCJE A ARGUMENTY

    round(3.14)

    ## [1] 3

z powodu domyślnej wartości argumentu:

round(x, digits = 0)

W takich przypadkach użytkownik może podać argument, ale nie musi:

    round(3.14, digits = 1)

    ## [1] 3.1

Dla uniknięcia problemów najlepiej zawsze dodawać nazwę argumentu, choć

    round(3.14, 1) 

daje ten sam wynik.

Pożyteczne argumenty tworzące wektory:

    rep()

    rep(c("abba", "beatles"), times=3)

    ## [1] "abba"    "beatles" "abba"    "beatles" "abba"    "beatles"
    
    rep(c("abba", "beatles"), each=3)
    
    ## [1] "abba"    "abba"    "abba"    "beatles" "beatles" "beatles"

`rep(c("abba", "beatles"), 3)` # 3 jest interpretowane jako "`times = 3`", bo times to pierwszy argument.

    ## [1] "abba"    "beatles" "abba"    "beatles" "abba"    "beatles"
    
        seq()
    
    seq(from = 0, to = 100, by = 20)
    
    ## [1]   0  20  40  60  80 100

**WŁASNE FUNKCJE**

    cls <- function() cat("\14") # clear screen

Funkcja zostaje zapisana w katalogu bieżącym (working directory); do stosowania w ten sposób:

    cls() # nie zapominać o pustych nawiasach!

Funkcja wracająca tabelę frekwencyjną danego wektora jako df:

    freq_df <- function(wektor){
    
      temp <- table(wektor)
      temp <- temp[order(-temp)]
      temp <- data.frame(V1 = names(temp), Freq = temp, row.names = NULL, stringsAsFactors = FALSE)
      return(temp)
    
    }
    
    freq_df(muza)

    ##        V1 Freq
    ## 1    abba    4
    ## 2 clapton    3
    ## 3 beatles    2
    ## 4    doda    1
    ## 5   łomot    1
    ## 6   zappa    1

**PAKIETY**

Pakiety trzeba raz zainstalować:

    install.packages()

a później na początku każdej sesji aktywować za pomocą:

    require()

lub

    library()

np.:

    install.packages("stringr") # "" konieczne
    require(stringr) # "" zbędne

Pakiety do prawie każdego celu można znaleźć dzięki stronie:

[www.rseek.org](http://www.rseek.org/)

**IMPORT & EKSPORT**

W miarę możliwości importować pliki z końcówkami:

    .txt (pliki tekstowe)
    .csv (tabele)

Do importu plików tekstowych służy funkcja

    scan()

    nasz_wektor <- scan("~/Documents/OneDrive/Warszawa_warsztaty/jakiś_tekst.txt", what=character())

Ścieżkę do pliku można też wybrać nawigując myszką za pomocą:

    file.choose()

    sciezka <- file.choose()

… a następnie:

    nasz_wektor <- scan(sciezka, what=character())

Domyślnie `scan()` wczytuje elementy podzielone spacją; z argumentem `sep = “\n”` wczyta linijki/akapity:

    nasz_wektor <- scan(sciezka, what=character(), sep="\n")

Wczytanie plików tabelowych:

    read.table() – tu potrzebne są różne argumenty
    
    read.csv2(), read.csv() – przeważnie najprostsze rozwiązanie (wersja europejska/amerykańska)
    
    read.delim(), read.delim2()

Do wczytania plików excelowych (.xlsx) jest pakiet “`xlsx`”.

Zapisanie plików na twardym dysku funkcjonuje analogicznie:

    pliki tekstowe:

    write()

    df:

    write.table()

    write.csv2(), write.csv()

Zapisane pliki można potem normalnie otworzyć np. Excelem itd.

Wczytanie/zapisanie plików w formacie r-owym (.RData):

    load()

    save()

Inne formaty danych: zob. niżej

**SKRYPTOWANIE**

Ważne elementy składni:

**if()**

    if (warunek) {jakiś fragment kodu}

    if () {} else {} – jeśli …, to …, w innym wypadku …

    if () {} else if {} else {} –

**Pętle**

    for (i in wektor) {} – pętla typu for; zmienna “i” przyjmuje po kolei wszystkie wartości elementów wektora; kod w klamrach jest wykonywany zgodnie z liczbą elementów wektora np.

    for (i in 1:5) {print(i)}

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5


`while (warunek) {jakiś fragment kodu}` – dopóki warunek jest spełniony, kod w klamrach jest wykonywany

    i <- 0
    while (i < 5) {
    i <- i + 1
    print(i)
    }

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5

**Przykład: funkcja “repetytor16”:**

Wstępnie trzeba stworzyć i wczytać tabelkę z trzema kolumnami tego typu:

         skt         pl lekcja
    1   gaja       słoń      1
    2   guru     mistrz      1
    3 nagara     miasto      2
    4   jāyā       żona      2
    5  kanyā dziewczyna      3
    6   aśva        koń      3
    7   agni      ogień      3
    8   nadī      rzeka      3

    sciezka <- file.choose()
    slowka <- read.csv2(file = sciezka, stringsAsFactors = FALSE)
    
    repetytor16 <- function(slowka_df) {
    
      cat("Dzień doberek!\nIle rund?\n") # scan() bez podanej ścieżki oczekuje podanie treści poprzez konsolę
      odp_rundy <- scan(what = integer(), nmax = 1, quiet = T)
    
      if (odp_rundy > nrow(slowka_df)) {odp_rundy <- nrow(slowka_df)} # ilość rund maks. = ilość wierszy w df
    
      rundy <- odp_rundy
    
      wybor_los <- sample(1:nrow(slowka_df), nrow(slowka_df)) # losowy porządek
    
      wybor_df <- slowka_df[wybor_los,] # uporządkowanie df zgodnie z los. porz.
    
      i <- 1 # licznik rund
      liczba_błędów <- 0 # licznik błędów
    
      while (i <= rundy) { 
        cat("Co znaczy: ", wybor_df[i, 1], "?\n", sep = "")
        odp <- scan(what = character(), nmax = 1, quiet = T)
        odp <- paste("\\b", odp, "\\b", sep = "") # przygotowanie wzorca do funkcji agrepl(), tzw. fuzzy matching
        if (agrepl(odp, wybor_df[i, 2], fixed=F) == TRUE) { # jeśli odp. jest prawidłowa, …
          cat("Zgadza się!\n")
          i <- i + 1
        } 
        else if (odp == "wystarczy") {stop ("No dobrze, ale tylko na dziś!")} # odp. "wystarczy" terminuje program
        else {
          cat("Błąd! Zapamiętaj:", wybor_df[i, 1], "znaczy", wybor_df[i, 2], "!\n")
          wybor_df$wyn_neg[i] <- wybor_df$wyn_neg[i] + 1
          i <- i + 1
          liczba_błędów <- liczba_błędów + 1
            if (rundy < nrow(wybor_df)) {rundy <- rundy + 1} # za karę dodatkowa runda, o ile to możliwe
        }
      }
    
      # obliczenie i podsumowanie wyniku
      wynik_proc <- round((rundy - liczba_błędów) / rundy * 100)
    
      cat("\n********************\nKoniec na teraz!\nLiczba rund:", rundy, 
          "| Liczba błędów:", liczba_błędów, 
          "| Procent:", wynik_proc)
    
      if (wynik_proc >= 90) {wynik <- "Świetnie!"} 
      else if (wynik_proc < 90 & wynik_proc >= 70) {wynik <- "Nieźle!"} 
      else if (wynik_proc < 70 & wynik_proc >= 50) {wynik <- "Mogło być lepiej!"} 
      else {wynik <- "Masakra!"}
    
      # wizualizacja wyniku
      pie(
        c(wynik_proc, 100 - wynik_proc),
        col = c("green", "red"),
        labels = NA,
        main = wynik
      )
    
    }

Teraz proszę: copy - paste - enter

Po zapamiętaniu funkcj można ją uruchomić w ten sposób:

    repetytor16(slowka) # "slowka" to przykładowa nazwa naszego df

Można oczywiście przygotować różne df na różne języki, np. sl_ang, sl_skt, sl_niem itd.

**WIZUALIZACJA**

Podstawowe wizualizacje statystyczne: proszę wypróbować na podstawie poszczególnych kolumn df mtcars!

    barplot()

    boxplot()

    hist()

Np.

    hist(mtcars$mpg)

    plot of chunk unnamed-chunk-33

Funkcja

    plot()

generuje wykres na podstawie układu współrzędnych (pierwszy wektor: x, drugi: y):

    plot(c(0,1), c(0,1))
    
    plot of chunk unnamed-chunk-34

Ważne argumenty:

    type: typ wykresu: p (points - standard), l (lines), n (nothing)
    pch: point character
    cex: wielkość (standard = 1)
    col: kolor

    plot(mtcars$mpg, mtcars$wt, pch = "☺︎", cex = 1.5, col = "violet")

    plot of chunk unnamed-chunk-35

Proszę eksperymentować!

Do gotowego układu można dodać kolejne elementy, np. za pomocą funkcji:

    text()

    points()

    lines()

    abline() – prosta linia pozioma (h=…) albo pionowa (v=…)

Np. tak dodajemy nazwy samochodów zamiast punktów:

`plot(mtcars$mpg, mtcars$wt, type = "n")` # na razie układ jest pusty

    text(mtcars$mpg, mtcars$wt, labels = row.names(mtcars), cex=0.5)

    plot of chunk unnamed-chunk-36

Często przydatne są też funkcje:

    density() – estymator jądrowy gęstości

    SMA() – simple moving average (w pakiecie TTR)

NA PRZYKŁADZIE Pana Tadeusza w ujęcie fonetycznym (inaczej niż na warsztatach z zachowaniem informacji nt. pochodzenia poszczególnych samogłosek).

Potrzebny będzie pakiet stringr (zob. wyżej) i plik `Pan_Tadeusz.txt`.

Najpierw wczytamy wszystkie linijki do jednego wektora:

    pt_linijki <- scan("TU/ODPOWIEDNIA/ŚCIEŻKA/Pan_Tadeusz.txt", what=character(), sep="\n")

Oto pierwsze trzy elementy:

    pt_linijki[1:3]
    
    ## [1] "Litwo! Ojczyzno moja! ty jesteś jak zdrowie:"
    ## [2] "Widzę i opisuję, bo tęsknię po tobie."       
    ## [3] "Panno święta, co Jasnej bronisz Częstochowy"

Zbudujemy kolejny wektor pt_linijki_sg, zawierający tylko samogłoski:

    pt_linijki_sg <- tolower(pt_linijki)
    pt_linijki_sg <- str_replace_all(pt_linijki_sg, "i(?=[aeouóąę])", "") # zlikwidowanie zmiękczających i
    pt_linijki_sg <- str_replace_all(pt_linijki_sg, "[^aeiouóyąę]", "")

zlikwidowanie wszystkich spółgłosek

Efekt:

    pt_linijki_sg[1:3]

    ## [1] "iooyooayeeaoe" "ięioiuęoęęooe" "aoęaoaeoięooy"

A teraz jeszcze wektor z liczbą samogłosek (czyli z liczbą znaków w każdym elemencie wektora):

    pt_liczba_sg <- nchar(pt_linijki_sg)

Przeważnie samogłosek jest, zgodnie z oczekiwaniem, 13:

pt_liczba_sg[1:10]

    ##  [1] 13 13 13 13 13 13 13 13 13 13

Ale według testu:

`sum(pt_liczba_sg != 13)` # tu TRUE jest traktowane jako liczba 1

    ## [1] 349

nie wszędzie, więc gdzieś kryją się jakieś nieścislości (powody: dyftongi, é itp.).

Wektor z sumą kumulatywną liczb samogłosek za chwilkę będzie przydatny:

    pt_numer_sg <- cumsum(pt_liczba_sg)
    pt_numer_sg[1:10]

    ##  [1]  13  26  39  52  65  78  91 104 117 130

I teraz skonstruujemy `df`:

    pt_df <- data.frame(pt_linijki, pt_linijki_sg, pt_liczba_sg, pt_numer_sg, stringsAsFactors = FALSE)

    pt_df[1:5, ]

    pt_linijki pt_linijki_sg pt_liczba_sg pt_numer_sg
                                    
    1 Litwo! Ojczyzno moja! ty jesteś jak zdrowie: iooyooayeeaoe           13          13
    2        Widzę i opisuję, bo tęsknię po tobie. ięioiuęoęęooe           13          26
    3  Panno święta, co Jasnej bronisz Częstochowy aoęaoaeoięooy           13          39
    4 Nowogródzki ochraniasz z jego wiernym ludem! ooóioaaeoeyue           13          52
    5 Jak mnie dziecko do zdrowia powróciłaś cudem aeeoooaoóiaue           13          65

Właściwie statystyki generujemy na podstawie wektora zawierającego jako elementy poszczególne samogłoski:

    pt_sg_wek <- paste(pt_df$pt_linijki_sg, collapse="") # skleić
    pt_sg_wek <- str_split(pt_sg_wek, "") # dzielić znak po znaku
    pt_sg_wek <- unlist(pt_sg_wek) # skonwertować listę w wektor

Zobaczmy:

    pt_sg_wek[1:10]
    
    ##  [1] "i" "o" "o" "y" "o" "o" "a" "y" "e" "e"
    
    length(pt_sg_wek)
    
    ## [1] 110629
    
    freq_df(pt_sg_wek)
    
    ##   V1  Freq
    ## 1  a 27069
    ## 2  e 21713
    ## 3  o 20260
    ## 4  i 11792
    ## 5  y 11608
    ## 6  u  6679
    ## 7  ę  4790
    ## 8  ą  4028
    ## 9  ó  2690

Gęstość samogł. “a”: podstawą obliczenia są “numery domu” obserwacji samogł. w wektorze:

    plot(density(which(pt_sg_wek == "a"), from=1, to=length(pt_sg_wek)))

    plot of chunk unnamed-chunk-45

Podobna metoda: średnia krocząca – sumuje “okienka” (tu: o długości 1000 elementów) wektora logicznego (TRUE ma numeryczną wartość 1); potrzebny pakiet TTR:

Wektory logiczne:

    temp_a <- pt_sg_wek == "a"
    temp_ę <- pt_sg_wek == "ę"
    temp_ą <- pt_sg_wek == "ą"

Wykres:

    plot(c(0,length(temp_a)), c(0, 1), 
    type="n", main = "Samogłoski\na – nieb., ę – czerw., ą = ziel.",  
    xlab= "PT", ylab= "Relatywna częstotliwość")
    lines(SMA(temp_a, n=1000)/max(SMA(temp_a, n=1000), na.rm=TRUE), col="blue")
    lines(SMA(temp_ę, n=1000)/max(SMA(temp_ę, n=1000), na.rm=TRUE), col="red")
    lines(SMA(temp_ą, n=1000)/max(SMA(temp_ą, n=1000), na.rm=TRUE), col="green")
    
    plot of chunk unnamed-chunk-46

Teraz zobaczmy, gdzie jest najwięcej „ę“:

    which(SMA(temp_ę, n=1000) == max(SMA(temp_ę, n=1000), na.rm = TRUE))
    
    ##  [1] 71607 71608 71609 71610 71611 71612 71613 71614 71615 71616 71617
    ## [12] 71618 71619 71620 71621 71622 71623 71624 71625 71626 71627 71628
    ## [23] 71629 71630 71631 71632 71633 71634 71635 71636 71637 71638 71639
    ## [34] 71640 71641 71642 71699 71700 71701

czyli w okienku ok. 76 linijek (1000/13) przed linijką, którą można ustalić w taki sposób:

    pt_df[pt_df$pt_numer_sg > 71701, ][1, ] # pierwsza linijka z grupy linijek spełniających warunek pt_numer_sg > 71701

    pt_linijki pt_linijki_sg pt_liczba_sg pt_numer_sg
    
    5543 Tak zrobiłem we Włoszech, kiedy pod opoką, aoieeoeeyoooą           13       71714

Powód zjawiska wydaje się być niestety prozaiczny: Sędzia w tej scenie odgrywa ważną rolę.

**PAKIETY NA PRZYSZŁOŚĆ**

Na prawie każdą potrzebę jest jakiś pakiet w R.
Do tworzenia bardziej wyrafinowych wykresów przydają się następujące pakiety:

    ggplot2 – eleganckie rozwiązania graficzne; przyjazna składnia
    shiny – interaktywne grafyki i aplikacji w przeglądarce

Do pracy z tekstami:

    tm - text mining: analiza korpusów itp.
    stylo – analiza stylometryczna

(Ogólne informacje nt. pracy nad danymi językowymi: https://cran.r-project.org/web/views/NaturalLanguageProcessing.html)
Sieci/grafy:

    igraph – analiza sieciowa

Do pracy z innymi formatami danych:

    Format 	Pakiet
    JSON 	rjson, RJSONIO
    XML 	XML
    SQL 	ODBC, JDBC

Do storzenia tego dokumentu używałem formatu R markdown i pakietu knitr (zob. http://rmarkdown.rstudio.com).






