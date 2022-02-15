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
## Przewodnik
#### Pierwsza zakładka
Pierwsza zakładka dzieli się na 3 kolejne.
Słuzą one do wizualizacji granic administracyjnych i możliwości pobierania ich w formacie Shapefile.
#### Druga zakładka
Druga zakładka dzieli się na 2 kolejne.
Służą one do transformacji pliku Shapefile lub wybranego na mapie poligonu na ortofotomapę.

