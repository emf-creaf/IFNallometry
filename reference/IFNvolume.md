# Volume allometry models

Static models for the wood volume (VCC, VSC, VLE & IAVC) by species and
province in the IFN

## Usage

``` r
IFNvolume(
  x,
  IFN = c(3, 2),
  FC = 1:6,
  code_missing = "99",
  provinceFromID = FALSE,
  DBHclasses = NULL,
  verbose = FALSE
)
```

## Arguments

- x:

  A data frame with tree records in rows and columns:

  - `ID`: String identifying forest stand.

  - `Species`: Species numeric used in IFN or a species name matching
    names given in
    [`species_ifn`](https://emf-creaf.github.io/IFNallometry/reference/species_ifn.md).

  - `DBH`: Tree diameter at breast height (in cm).

  - `H`: Tree height (in m).

  - `N`: Tree density factor (in ind/ha).

  - `Provincia`: Numeric or character province code (optional). If not
    provided, the user should set `provinceFromID = TRUE`.

  - `FC`: The cubic content form for each tree (optional). In this case,
    the value of parameter `FC` is overriden.

- IFN:

  Integer or integer pair to indicate order of preferred forest
  inventory: (`IFN=2` indicates using IFN-2 only, `IFN=3` indicates
  using IFN-3 only, and `IFN=c(3,2)` indicates using IFN3 and if not
  available IFN2)

- FC:

  Vector of integers to indicate preferred cubic content forms (1 to 6)

  - Forma 1. Arboles fusiformes prácticamente en todo su fuste, con
    troncos maderables, limpios y derechos de más de 6 m, flecha
    inferior al 1 y diámetro normal mayor de 20 cm.

  - Forma 2. Arboles que cumplan las cuatro condiciones siguientes: ser
    fusiformes, tener troncos maderables de 4 o más metros, ramificarse
    por la parte superior y no pertenecer a la forma 1.

  - Forma 3. Arboles fusiformes pequeños, en los que el diámetro del
    fuste de 75 mm queda por debajo de los 4 m de altura.

  - Forma 4. Árboles cuyo tronco principal se ramifica antes de los 4 m
    de altura y que pertenezcan a algunas de las siguientes especies 07,
    12, 16, 23, 41, 42, 43, 44, 45, 46, 47, 48, 49, 55, 56, 57, 66, 67,
    71, 72, 74, 75, 79 y 94.

  - Forma 5. Arboles cuyo tronco principal es tortuoso, está dañado o es
    muy ramoso, por lo que no admite la clasificación en formas 1, 2
    o 3. También pies de altura de fuste menor de 4 m si son de especies
    diferentes a las de los códigos 4 y 6.

  - Forma 6. Árboles descabezados o trasmochos a los que se les ha
    cortado la parte superior del tronco y las ramas en puntos próximos
    a su inserción en el tronco y que pertenezcan a algunas de las
    siguientes especies: 41, 42, 43, 55, 56, 71, 72 y 94.

- code_missing:

  Species code to use when equations are not available for the species
  recorded ("99" is Otras frondosas)

- provinceFromID:

  A flag to indicate that province code should be extracted from the
  first characters of column `ID` in `x`.

- DBHclasses:

  A numeric vector of DBH class limits (see breaks in function
  [`cut`](https://rdrr.io/r/base/cut.html)), used to group results by
  DBH class (in addition to species and plot). If `DBHclasses = NULL`
  then grouping on the basis of DBH classes is not performed.

- verbose:

  A flag to indicate console output of the volume calculation process

## Value

If `DBHclasses = NULL`, a data frame with as many rows as tree records
in `x` and columns:

- `ID`: Forest plot identifier, as given in input.

- `Species`: Species code or species name given in input.

- `Name`: Species name given in volume allometry equations.

- `FC`: Cubic content form used to calculate volume.

- `VCC`: Volumen con corteza (m3/ha).

- `VSC`: Volumen sin corteza (m3/ha).

- `VLE`: Volumen de leñas (m3/ha).

- `IAVC`: Incremento anual del volumen con corteza (m3/ha/yr).

If `DBHclasses != NULL` then an extra column `DBHclass` is given and the
data frame has less rows than tree records in `x`.

## Details

The volumetric equation used for each tree record depends on province,
species, cubic content form and volume parameter (VCC, VSC, VLE and
IAVC). Volumes are given as per hectare (i.e. the result of the
volumetric equation is multiplied by the density 'N'). If cubic content
form is not given in 'x', then the function iterates over the values of
'FC' until an equation is available.

Species can be supplied as IFN codes or species names. In the latter
case, the function will convert them to codes using
[`species_ifn`](https://emf-creaf.github.io/IFNallometry/reference/species_ifn.md)
data table. When species are provided as character names, both exact
matching and function
[`startsWith`](https://rdrr.io/r/base/startsWith.html) are used.

## See also

[`IFNbiomass`](https://emf-creaf.github.io/IFNallometry/reference/IFNbiomass.md)
[`IFNvolume_medfate`](https://emf-creaf.github.io/IFNallometry/reference/IFNvolume_medfate.md)

## Examples

``` r
data(example_tree_data)


# Calculate volume using plot ID to extract province
IFNvolume(example_tree_data, provinceFromID = TRUE)
#>       ID Species             Name FC       VCC       VSC          VLE
#> 1  80001      22   Pinus uncinata  1  3.602559 2.8602086 -0.035419361
#> 2  80001      22   Pinus uncinata  1  3.229656 2.6906360  0.033717806
#> 3  80001      22   Pinus uncinata  1  2.805269 2.3210295  0.018885424
#> 4  80001      22   Pinus uncinata  1  2.150192 1.7501041 -0.001320525
#> 5  80001      22   Pinus uncinata  1  4.176124 3.5141919  0.071731231
#> 6  80001      22   Pinus uncinata  1  2.441344 1.8466562 -0.061216067
#> 7  80001      22   Pinus uncinata  1  2.905217 2.2516241 -0.051457746
#> 8  80001      22   Pinus uncinata  1  4.895048 3.1326708 -0.333478326
#> 9  80001      22   Pinus uncinata  1  3.412549 2.8498577  0.040532336
#> 10 80001      22   Pinus uncinata  1  4.618006 3.7459660 -0.009125178
#> 11 80001      22   Pinus uncinata  1  3.070085 2.5516873  0.027980097
#> 12 80001      22   Pinus uncinata  1  4.893245 3.9859626 -0.001397636
#> 13 80001      22   Pinus uncinata  1  2.698989 2.2284357  0.015385395
#> 14 80001      22   Pinus uncinata  1  2.958217 2.4542582  0.024073083
#> 15 80001      22   Pinus uncinata  1  3.850793 3.2312255  0.057896348
#> 16 80001      22   Pinus uncinata  1  2.785409 2.3037275  0.018224847
#> 17 80001      22   Pinus uncinata  1  2.216981 1.6507447 -0.065674920
#> 18 80002      76         Acer sp.  1  3.008303 2.4968822  0.210210873
#> 19 80002      22   Pinus uncinata  1  4.522909 3.6630368 -0.011735565
#> 20 80002      21 Pinus sylvestris  1  7.515290 6.1285682  0.103398584
#> 21 80002      21 Pinus sylvestris  1  5.488614 4.4270252  0.057079836
#> 22 80002      22   Pinus uncinata  1  3.265693 2.7220122  0.035040419
#> 23 80002      21 Pinus sylvestris  1  6.498953 5.2736239  0.077884292
#> 24 80002      21 Pinus sylvestris  1  5.949039 4.8124233  0.065996936
#> 25 80002      21 Pinus sylvestris  1  8.994729 7.3790254  0.148760060
#> 26 80002      21 Pinus sylvestris  1  3.255691 2.5676510  0.027225420
#> 27 80002      22   Pinus uncinata  1  3.058556 2.5416467  0.027573032
#> 28 80002      21 Pinus sylvestris  1  2.890342 2.2649519  0.024454330
#> 29 80002      21 Pinus sylvestris  1  8.255841 6.7536175  0.124886446
#> 30 80002      21 Pinus sylvestris  1  6.291642 5.0996424  0.073244759
#> 31 80002      21 Pinus sylvestris  1 10.138500 8.3506039  0.190510143
#> 32 80002      22   Pinus uncinata  1  3.941361 3.3100117  0.061666937
#> 33 81073      21 Pinus sylvestris  1  3.215746 2.3820362  0.043456884
#> 34 81073      24 Pinus halepensis  1  2.067493 1.4648758  0.104449621
#> 35 81073      24 Pinus halepensis  1  1.963343 1.2892007  0.103312911
#> 36 81073      24 Pinus halepensis  1  2.098458 1.3892741  0.108565936
#> 37 81073      24 Pinus halepensis  1  2.126528 1.5093168  0.108000778
#> 38 81073      24 Pinus halepensis  1  2.671292 1.8144627  0.132442012
#> 39 81073      21 Pinus sylvestris  1  1.084917 0.6311426  0.047386252
#> 40 81073      24 Pinus halepensis  1  4.028973 2.8281275  0.199406047
#>          IAVC
#> 1  0.12176755
#> 2  0.07381033
#> 3  0.06887026
#> 4  0.06097990
#> 5  0.08434162
#> 6  0.10679360
#> 7  0.11282913
#> 8  0.36244217
#> 9  0.07589768
#> 10 0.13449377
#> 11 0.07196868
#> 12 0.13788410
#> 13 0.06761197
#> 14 0.07066619
#> 15 0.08079739
#> 16 0.06863576
#> 17 0.10384867
#> 18 0.42381477
#> 19 0.13331653
#> 20 0.19966278
#> 21 0.16119478
#> 22 0.07422360
#> 23 0.18085091
#> 24 0.17027046
#> 25 0.22532385
#> 26 0.11437473
#> 27 0.07183487
#> 28 0.10627122
#> 29 0.21276301
#> 30 0.17689534
#> 31 0.24376298
#> 32 0.08179202
#> 33 0.16339097
#> 34 0.07982800
#> 35 0.10865772
#> 36 0.11231471
#> 37 0.08134209
#> 38 0.12771191
#> 39 0.11191915
#> 40 0.16351333

# Define province from first characters of ID
example_tree_data <- example_tree_data |>
   dplyr::mutate(Province = substr(ID, 1, nchar(ID)-4))

# Calculate volume (requires column 'province' or 'Province')
IFNvolume(example_tree_data)
#>       ID Species             Name FC       VCC       VSC          VLE
#> 1  80001      22   Pinus uncinata  1  3.602559 2.8602086 -0.035419361
#> 2  80001      22   Pinus uncinata  1  3.229656 2.6906360  0.033717806
#> 3  80001      22   Pinus uncinata  1  2.805269 2.3210295  0.018885424
#> 4  80001      22   Pinus uncinata  1  2.150192 1.7501041 -0.001320525
#> 5  80001      22   Pinus uncinata  1  4.176124 3.5141919  0.071731231
#> 6  80001      22   Pinus uncinata  1  2.441344 1.8466562 -0.061216067
#> 7  80001      22   Pinus uncinata  1  2.905217 2.2516241 -0.051457746
#> 8  80001      22   Pinus uncinata  1  4.895048 3.1326708 -0.333478326
#> 9  80001      22   Pinus uncinata  1  3.412549 2.8498577  0.040532336
#> 10 80001      22   Pinus uncinata  1  4.618006 3.7459660 -0.009125178
#> 11 80001      22   Pinus uncinata  1  3.070085 2.5516873  0.027980097
#> 12 80001      22   Pinus uncinata  1  4.893245 3.9859626 -0.001397636
#> 13 80001      22   Pinus uncinata  1  2.698989 2.2284357  0.015385395
#> 14 80001      22   Pinus uncinata  1  2.958217 2.4542582  0.024073083
#> 15 80001      22   Pinus uncinata  1  3.850793 3.2312255  0.057896348
#> 16 80001      22   Pinus uncinata  1  2.785409 2.3037275  0.018224847
#> 17 80001      22   Pinus uncinata  1  2.216981 1.6507447 -0.065674920
#> 18 80002      76         Acer sp.  1  3.008303 2.4968822  0.210210873
#> 19 80002      22   Pinus uncinata  1  4.522909 3.6630368 -0.011735565
#> 20 80002      21 Pinus sylvestris  1  7.515290 6.1285682  0.103398584
#> 21 80002      21 Pinus sylvestris  1  5.488614 4.4270252  0.057079836
#> 22 80002      22   Pinus uncinata  1  3.265693 2.7220122  0.035040419
#> 23 80002      21 Pinus sylvestris  1  6.498953 5.2736239  0.077884292
#> 24 80002      21 Pinus sylvestris  1  5.949039 4.8124233  0.065996936
#> 25 80002      21 Pinus sylvestris  1  8.994729 7.3790254  0.148760060
#> 26 80002      21 Pinus sylvestris  1  3.255691 2.5676510  0.027225420
#> 27 80002      22   Pinus uncinata  1  3.058556 2.5416467  0.027573032
#> 28 80002      21 Pinus sylvestris  1  2.890342 2.2649519  0.024454330
#> 29 80002      21 Pinus sylvestris  1  8.255841 6.7536175  0.124886446
#> 30 80002      21 Pinus sylvestris  1  6.291642 5.0996424  0.073244759
#> 31 80002      21 Pinus sylvestris  1 10.138500 8.3506039  0.190510143
#> 32 80002      22   Pinus uncinata  1  3.941361 3.3100117  0.061666937
#> 33 81073      21 Pinus sylvestris  1  3.215746 2.3820362  0.043456884
#> 34 81073      24 Pinus halepensis  1  2.067493 1.4648758  0.104449621
#> 35 81073      24 Pinus halepensis  1  1.963343 1.2892007  0.103312911
#> 36 81073      24 Pinus halepensis  1  2.098458 1.3892741  0.108565936
#> 37 81073      24 Pinus halepensis  1  2.126528 1.5093168  0.108000778
#> 38 81073      24 Pinus halepensis  1  2.671292 1.8144627  0.132442012
#> 39 81073      21 Pinus sylvestris  1  1.084917 0.6311426  0.047386252
#> 40 81073      24 Pinus halepensis  1  4.028973 2.8281275  0.199406047
#>          IAVC
#> 1  0.12176755
#> 2  0.07381033
#> 3  0.06887026
#> 4  0.06097990
#> 5  0.08434162
#> 6  0.10679360
#> 7  0.11282913
#> 8  0.36244217
#> 9  0.07589768
#> 10 0.13449377
#> 11 0.07196868
#> 12 0.13788410
#> 13 0.06761197
#> 14 0.07066619
#> 15 0.08079739
#> 16 0.06863576
#> 17 0.10384867
#> 18 0.42381477
#> 19 0.13331653
#> 20 0.19966278
#> 21 0.16119478
#> 22 0.07422360
#> 23 0.18085091
#> 24 0.17027046
#> 25 0.22532385
#> 26 0.11437473
#> 27 0.07183487
#> 28 0.10627122
#> 29 0.21276301
#> 30 0.17689534
#> 31 0.24376298
#> 32 0.08179202
#> 33 0.16339097
#> 34 0.07982800
#> 35 0.10865772
#> 36 0.11231471
#> 37 0.08134209
#> 38 0.12771191
#> 39 0.11191915
#> 40 0.16351333

# Groups the result by DBH clases
IFNvolume(example_tree_data, provinceFromID = TRUE,
          DBHclasses = seq(0, 120, by=5))
#>   "ID" "Species" "Name" "DBHclass" "FC"      VCC      VSC      VLE     IAVC
#> 1   ID   Species   Name   DBHclass   FC 159.0419 126.4681 1.812554 5.328669
```
