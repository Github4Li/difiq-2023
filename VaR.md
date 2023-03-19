Risques extrêmes et application à la mesure du risque de marché
================
Pierre Clauss
Mars 2023

*Ce document R Markdown a pour objet la résolution des exercices 1.1 et
2.1 du cours.*

## Préambule

Je précise en préambule les 3 étapes nécessaires pour la réussite d’un
projet de data science :

1.  données : (i) importation, (ii) wrangling et (iii) visualisation (ou
    appelée encore *analyse exploratoire des données*)
2.  modélisation
3.  communication des résultats

L’univers du package **tidyverse** est essentiel pour réaliser ces 3
étapes avec R aujourd’hui.

``` r
library(tidyverse)
```

## 1 Données

### 1.1 Importation

Pour les 2 exercices, les données sont les mêmes. J’importe ces données
à l’aide du package **readxl**, qui gère les fichiers Excel parfaitement
(décimales, pourcentages, valeurs manquantes), avec la fonction
`read_xlsx()`.

``` r
library(readxl)
(renta <- read_xlsx(
  "data.xlsx",
  sheet = "VaR",
  skip = 4,
  col_names = c("Date", "France", "BRIC", "US_Corporate_Bonds")
))
```

    ## # A tibble: 4,684 x 4
    ##    Date                   France     BRIC US_Corporate_Bonds
    ##    <dttm>                  <dbl>    <dbl>              <dbl>
    ##  1 2003-10-01 00:00:00  0.0238    0.0164            0.000210
    ##  2 2003-10-02 00:00:00 -0.00182   0.0168           -0.00342 
    ##  3 2003-10-03 00:00:00  0.0257    0.0159           -0.0102  
    ##  4 2003-10-06 00:00:00 -0.000820  0.0150            0.00352 
    ##  5 2003-10-07 00:00:00  0.000733  0.0135           -0.00471 
    ##  6 2003-10-08 00:00:00  0.00262   0.0197            0.000419
    ##  7 2003-10-09 00:00:00  0.0161    0.00885          -0.00213 
    ##  8 2003-10-10 00:00:00  0.000565 -0.00139           0.00297 
    ##  9 2003-10-13 00:00:00  0.00356   0.0111            0.000496
    ## 10 2003-10-14 00:00:00 -0.00218  -0.00183          -0.00471 
    ## # ... with 4,674 more rows

Les données sont issues de Quandl. Vous pouvez remarquer dans la cellule
B4 du fichier Excel la fonction Quandl qui permet d’importer les données
*=QSERIES(Index_1 : Index_3 ; Start_date : End_date ; “daily” ; “asc” ;
“rdiff”)*.

Les données sont un échantillon de rentabilités quotidiennes de deux
indices actions du Nasdaq (France et BRIC) et d’un indice Merrill Lynch
obligataire d’entreprises US. L’échantillon commence le 1er octobre 2003
et se termine à la date du premier cours.

### 1.2 Démêlage (wrangling en anglais)

“Tidying and transforming are called *wrangling*, because getting your
data in a form that’s natural to work with often feels like a fight”
[**R for Data Science**](https://r4ds.had.co.nz/introduction.html)
(Grolemund G. and Wickham H.).

Je peux à l’aide du package **DataExplorer** obtenir un résumé des
données et évaluer si je peux les considérer comme **tidy**. Je vais
devoir enlever les valeurs manquantes.

``` r
library(DataExplorer)
renta <- renta %>% select(-"Date")
plot_intro(renta)
```

![](VaR_files/figure-gfm/wrangling-1.png)<!-- -->

``` r
(renta <- renta %>% drop_na())
```

    ## # A tibble: 4,534 x 3
    ##       France     BRIC US_Corporate_Bonds
    ##        <dbl>    <dbl>              <dbl>
    ##  1  0.0238    0.0164            0.000210
    ##  2 -0.00182   0.0168           -0.00342 
    ##  3  0.0257    0.0159           -0.0102  
    ##  4 -0.000820  0.0150            0.00352 
    ##  5  0.000733  0.0135           -0.00471 
    ##  6  0.00262   0.0197            0.000419
    ##  7  0.0161    0.00885          -0.00213 
    ##  8  0.000565 -0.00139           0.00297 
    ##  9  0.00356   0.0111            0.000496
    ## 10 -0.00218  -0.00183          -0.00471 
    ## # ... with 4,524 more rows

``` r
plot_intro(renta)
```

![](VaR_files/figure-gfm/wrangling-2.png)<!-- -->

### 1.3 Visualisation

Les statistiques de base sont résumées par le tableau et les graphiques
ci-dessous. Nous pouvons observer un fait stylisé très important des
rentabilités d’indices de marché, à savoir la leptokurticité de leur
densité.

``` r
summary(renta)
```

    ##      France                BRIC            US_Corporate_Bonds  
    ##  Min.   :-0.1367993   Min.   :-0.1226388   Min.   :-0.0375754  
    ##  1st Qu.:-0.0060935   1st Qu.:-0.0057636   1st Qu.:-0.0014726  
    ##  Median : 0.0007368   Median : 0.0010324   Median : 0.0003175  
    ##  Mean   : 0.0004332   Mean   : 0.0005314   Mean   : 0.0001992  
    ##  3rd Qu.: 0.0075015   3rd Qu.: 0.0076174   3rd Qu.: 0.0019813  
    ##  Max.   : 0.1236734   Max.   : 0.1449443   Max.   : 0.0198546

``` r
plot_density(renta)
```

![](VaR_files/figure-gfm/viz%20data-1.png)<!-- -->

``` r
plot_qq(renta)
```

![](VaR_files/figure-gfm/viz%20data-2.png)<!-- -->

## 2 Modélisation

Les VaR sont soit non-paramétriques (historique et bootstrap) soit
paramétriques (Gaussienne, Skew Student, GEV et GPD).

### 2.1 Résolution de *l’exercice 1.1* du cours

``` r
library(scales)
library(sn)

VaR_classiques <- function(data, alpha, boot = 100)
{
  #VaR Historique
  Hist <- quantile(data, probs = alpha)
  Hist <- unname(Hist)
  Hist <- percent(Hist, 0.01)
  
  #VaR Bootstrap
  x <- numeric(boot)
  for (j in 1:boot)
  {
    databoot <- sample(data, replace = T)
    x[j] <- quantile(databoot, probs = alpha)
  }
  Boot <- mean(x)
  Boot <- percent(Boot, 0.01)
  
  # VaR Gaussienne
  Gauss <- mean(data) + sd(data) * qnorm(alpha, 0, 1)
  Gauss <- percent(Gauss, 0.01)
  
  # VaR skew Student
  esti <- st.mple(y = data)
  Skt <- qst(alpha, esti$dp["xi"], esti$dp["omega"], esti$dp["alpha"], esti$dp["nu"])
  Skt <- percent(Skt, 0.01)
  
  return(c(Historique = Hist, Bootstrap = Boot, Gaussienne = Gauss, Skew_Student = Skt))
}
```

Voici ci-dessous les VaR demandées dans *l’exercice 1.1* pour les 3
indices avec alpha = 1%.

``` r
library(pander)
l_1 <- list(data = renta, alpha = 0.01)
VaR_1 <-  pmap(l_1, VaR_classiques)
pander(VaR_1)
```

-   **France**:

    | Historique | Bootstrap | Gaussienne | Skew_Student |
    |:----------:|:---------:|:----------:|:------------:|
    |   -4.42%   |  -4.40%   |   -3.42%   |    -4.39%    |

-   **BRIC**:

    | Historique | Bootstrap | Gaussienne | Skew_Student |
    |:----------:|:---------:|:----------:|:------------:|
    |   -4.00%   |  -3.97%   |   -3.21%   |    -4.10%    |

-   **US_Corporate_Bonds**:

    | Historique | Bootstrap | Gaussienne | Skew_Student |
    |:----------:|:---------:|:----------:|:------------:|
    |   -0.81%   |  -0.81%   |   -0.71%   |    -0.83%    |

<!-- end of list -->

Voici ci-dessous les VaR demandées dans *l’exercice 1.1* pour les 3
indices avec alpha = 0.1%.

``` r
l_01 <- list(data = renta, alpha = 0.001)
VaR_01 <- pmap(l_01, VaR_classiques)
pander(VaR_01)
```

-   **France**:

    | Historique | Bootstrap | Gaussienne | Skew_Student |
    |:----------:|:---------:|:----------:|:------------:|
    |   -7.54%   |  -7.79%   |   -4.55%   |   -10.29%    |

-   **BRIC**:

    | Historique | Bootstrap | Gaussienne | Skew_Student |
    |:----------:|:---------:|:----------:|:------------:|
    |   -8.54%   |  -8.54%   |   -4.28%   |    -9.13%    |

-   **US_Corporate_Bonds**:

    | Historique | Bootstrap | Gaussienne | Skew_Student |
    |:----------:|:---------:|:----------:|:------------:|
    |   -1.73%   |  -1.81%   |   -0.95%   |    -1.54%    |

<!-- end of list -->

### 2.2 Résolution de *l’exercice 2.1* du cours

Pour définir la VaR GPD, il est nécessaire de déterminer le seuil à
partir duquel il est raisonnable de penser que les extrêmes suivent une
loi GPD. Cela se fait grâce au mean-excess plot : le seuil optimal est
la valeur à partir de laquelle la tendance est croissante.

``` r
library(evir)

meplot(-renta$France[renta$France < 0])
```

![](VaR_files/figure-gfm/var%20TVE-1.png)<!-- -->

``` r
meplot(-renta$BRIC[renta$BRIC < 0])
```

![](VaR_files/figure-gfm/var%20TVE-2.png)<!-- -->

``` r
meplot(-renta$US_Corporate_Bonds[renta$US_Corporate_Bonds < 0])
```

![](VaR_files/figure-gfm/var%20TVE-3.png)<!-- -->

``` r
VaR_TVE <- function(data, alpha, bloc = 21, seuil = 0.01)
{
  # VaR GEV
  g1 <- gev(-data, bloc)
  alphaGEV <- 1 - bloc * alpha
  GEV <- -qgev(alphaGEV, g1$par.ests["xi"], g1$par.ests["mu"], g1$par.ests["sigma"])
  GEV <- unname(GEV)
  GEV <- percent(GEV, 0.01)
  
  # VaR GPD
  g2 <- gpd(-data, seuil)
  p <- length(g2$data) / length(data)
  GPD <- -qgpd(1 - alpha / p, g2$par.ests["xi"], seuil, g2$par.ests["beta"])
  GPD <- unname(GPD)
  GPD <- percent(GPD, 0.01)
  
  return(c(GEV = GEV, GPD = GPD))
}
```

Voici ci-dessous les VaR TVE demandées dans *l’exercice 2.1* pour les 3
indices avec alpha = 1%.

``` r
l_1 <- list(data = renta, alpha = 0.01, seuil = c(0.03, 0.03, 0.006))
VaR_1 <-  pmap(l_1, VaR_TVE)
pander(VaR_1)
```

-   **France**:

    |  GEV   |  GPD   |
    |:------:|:------:|
    | -3.28% | -4.39% |

-   **BRIC**:

    |  GEV   |  GPD   |
    |:------:|:------:|
    | -3.13% | -4.05% |

-   **US_Corporate_Bonds**:

    |  GEV   |  GPD   |
    |:------:|:------:|
    | -0.70% | -0.82% |

<!-- end of list -->

Voici ci-dessous les VaR TVE demandées dans *l’exercice 2.1* pour les 3
indices avec alpha = 0.1%.

``` r
l_01 <- list(data = renta, alpha = 0.001, seuil = c(0.03, 0.03, 0.006))
VaR_01 <- pmap(l_01, VaR_TVE)
pander(VaR_01)
```

-   **France**:

    |  GEV   |  GPD   |
    |:------:|:------:|
    | -7.40% | -8.13% |

-   **BRIC**:

    |  GEV   |  GPD   |
    |:------:|:------:|
    | -6.93% | -8.65% |

-   **US_Corporate_Bonds**:

    |  GEV   |  GPD   |
    |:------:|:------:|
    | -1.39% | -1.77% |

<!-- end of list -->
