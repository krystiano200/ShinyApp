Aplikacja pozwalająca przeglądać, pobierać i przetwarzać dane z GUGIK
========================

## Wymagania
Aby aplikacja działała potrzebujemy zainstalowane R, RStudio oraz pakiety:
```r
install.packages("rgugik")
install.packages("shiny")
install.packages("shinythemes")
install.packages("sf")
install.packages("stars")
install.packages("mapview")
install.packages("mapedit")
install.packages("dplyr")
install.packages("stringr")

```
## Jak odtworzyć 

```r
library(shiny)
runGitHub("ShinyApp", "krystiano200")
```

## Przewodnik
#### Pierwsza zakładka
Pierwsza zakładka dzieli się na 3 kolejne.
Słuzą one do wizualizacji granic administracyjnych i możliwości pobierania ich w formacie Shapefile.
#### Druga zakładka
Druga zakładka "Ortofotomapa" dzieli się na 2 kolejne.
Służą one do transformacji pliku Shapefile lub wybranego na mapie poligonu na ortofotomapę.

### Trzecia zakładka
Trzecia zakładka, czyli numeryczny model terenu dzieli się na dwie kolejne.
Istnieje zakładka "Shapefile" i "Z mapy".
W tej części możemy wgrać plik shp lub narysować poligon na mapie,
aby dostać w ich miejsce numeryczny model terenu.

### Wskazówki
Przy wgrywaniu plików shapefile należy zaznaczyć wszystkie 4 pliki.

Im większe poligony tym większe pliki.

Istnieją miejsca, które zwrócą Error


