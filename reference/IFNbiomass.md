# Biomass allometry models

Static models for the biomass (kg/ha) of stem, roots, branches,
leaves/needles and bark of species in the IFN

## Usage

``` r
IFNbiomass(x, as.CO2 = FALSE, area = NA, DBHclasses = NULL, verbose = FALSE)
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

- as.CO2:

  Flag to indicate output as kg of CO2 / ha instead of kg of dry weight
  / ha. Percentage of carbon per dry weight biomass by species are taken
  from Montero et al. (2005) (in turn, from Ibáñez et al. 2002).

- area:

  Either 'Atlantic' or 'Mediterranean' to specify allometric equations
  specific to the area (for Pinus pinaster)

- DBHclasses:

  A numeric vector of DBH class limits (see breaks in function `cut`),
  used to group results by DBH class (in addition to species and plot).
  If `DBHclasses = NULL` then grouping on the basis of DBH classes is
  not performed.

- verbose:

  A flag to indicate console output of the biomass calculation process

## Value

If `DBHclasses = NULL`, function `IFNbiomass` returns a data frame with
as many rows as tree records in `x` and columns 'ID', 'Species',
'Roots', 'Stem', 'Branches', 'Leaves', 'Needles', 'Bark', 'Aerial' and
'Total'. If `DBHclasses != NULL` then an extra column `DBHclass` is
given and the data frame has less rows than tree records in `x`.

## Details

Function `IFNbiomass` determines the allometric equation for each
biomass fraction (i.e. compartment) using species code. All equations
are calculated form height and diameter at breast height (DBH) of felled
trees. Biomass values from equations refer to individual trees, but the
function multiplies the result by the density of individuals 'N', so the
resulting value is in units of kg/ha. Biomass of branches is the result
of adding the result of equations calibrated for different branch
diameters.

When species are provided as character names, both exact matching and
function [`startsWith`](https://rdrr.io/r/base/startsWith.html) are
used.

## References

Diéguez-Aranda, U., A. Rojo Alboreca, F. Castedo-Dorado, J. G. Álvarez
González, M. Barrio-Anta, F. Crecente-Campo, J. M. González González, C.
Pérez-Cruzado, R. Rodríguez Soalleiro, C. A. López-Sánchez, M. Á.
Balboa-Murias, J. J. Gorgoso Varela, and F. Sánchez Rodríguez (2009)
Herramientas selvícolas para la gestión forestal sostenible en Galicia.
Dirección Xeral de Montes, Consellería do Medio Rural, Xunta de Galicia.

Ibáñez JJ, Vayreda J., Gracia C. (2002) Metodología complementaria al
Inventario Forestal Nacional en Catalunya. En: Bravo F.; del Río M.; del
Peso C. (eds.) El inventario Forestal Nacional. Elemento clave para la
gestión forestal sostenible: 67-77. Fundación General de la Universidad
de Valladolid Montero G, Ruiz-Peinado R, Muñoz M (2005) Producción de
biomasa y fijación de CO2 por los bosques españoles. Monografias INIA:
Serie Forestal nº 13.

Ruiz-Peinado, R., M. Rio, and G. Montero (2011) New models for
estimating the carbon sink capacity of Spanish. Forest Systems
20:176–188.

Ruiz-Peinado, R., G. Montero, and M. Del Rio (2012) Biomass models to
estimate carbon stocks for hardwood tree species. Forest Systems 21:42.

## See also

[`IFNvolume`](https://emf-creaf.github.io/IFNallometry/reference/IFNvolume.md)

## Examples

``` r
data(example_tree_data)

IFNbiomass(example_tree_data)
#>       ID Species             Name SpeciesAllom             NameAllom     Roots
#> 1  80001      22   Pinus uncinata           22        Pinus uncinata 2194.4771
#> 2  80001      22   Pinus uncinata           22        Pinus uncinata 2328.0410
#> 3  80001      22   Pinus uncinata           22        Pinus uncinata 1559.6269
#> 4  80001      22   Pinus uncinata           22        Pinus uncinata 1533.6336
#> 5  80001      22   Pinus uncinata           22        Pinus uncinata 2919.5803
#> 6  80001      22   Pinus uncinata           22        Pinus uncinata 1153.0511
#> 7  80001      22   Pinus uncinata           22        Pinus uncinata 1514.2819
#> 8  80001      22   Pinus uncinata           22        Pinus uncinata 1456.9642
#> 9  80001      22   Pinus uncinata           22        Pinus uncinata 1859.9700
#> 10 80001      22   Pinus uncinata           22        Pinus uncinata 2531.6258
#> 11 80001      22   Pinus uncinata           22        Pinus uncinata 1431.8446
#> 12 80001      22   Pinus uncinata           22        Pinus uncinata 2722.1474
#> 13 80001      22   Pinus uncinata           22        Pinus uncinata 1572.7055
#> 14 80001      22   Pinus uncinata           22        Pinus uncinata 2102.5721
#> 15 80001      22   Pinus uncinata           22        Pinus uncinata 2132.9886
#> 16 80001      22   Pinus uncinata           22        Pinus uncinata 1546.6029
#> 17 80001      22   Pinus uncinata           22        Pinus uncinata 1204.1026
#> 18 80002      76   Acer campestre           55 Fraxinus angustifolia 5348.0008
#> 19 80002      22   Pinus uncinata           22        Pinus uncinata 2336.0206
#> 20 80002      21 Pinus sylvestris           21      Pinus sylvestris 2869.4929
#> 21 80002      21 Pinus sylvestris           21      Pinus sylvestris 1883.2628
#> 22 80002      22   Pinus uncinata           22        Pinus uncinata 1768.4814
#> 23 80002      21 Pinus sylvestris           21      Pinus sylvestris 2057.8002
#> 24 80002      21 Pinus sylvestris           21      Pinus sylvestris 1877.3822
#> 25 80002      21 Pinus sylvestris           21      Pinus sylvestris 2761.5593
#> 26 80002      21 Pinus sylvestris           21      Pinus sylvestris 1135.7003
#> 27 80002      22   Pinus uncinata           22        Pinus uncinata 1425.5988
#> 28 80002      21 Pinus sylvestris           21      Pinus sylvestris 1233.7026
#> 29 80002      21 Pinus sylvestris           21      Pinus sylvestris 2039.3860
#> 30 80002      21 Pinus sylvestris           21      Pinus sylvestris 2076.2972
#> 31 80002      21 Pinus sylvestris           21      Pinus sylvestris 3121.8023
#> 32 80002      22   Pinus uncinata           22        Pinus uncinata 1824.5090
#> 33 81073      21 Pinus sylvestris           21      Pinus sylvestris 1638.7007
#> 34 81073      24 Pinus halepensis           24      Pinus halepensis  818.6098
#> 35 81073      24 Pinus halepensis           24      Pinus halepensis  496.7730
#> 36 81073      24 Pinus halepensis           24      Pinus halepensis  432.0871
#> 37 81073      24 Pinus halepensis           24      Pinus halepensis  852.1122
#> 38 81073      24 Pinus halepensis           24      Pinus halepensis  717.8921
#> 39 81073      21 Pinus sylvestris           21      Pinus sylvestris  754.1557
#> 40 81073      24 Pinus halepensis           24      Pinus halepensis  818.6098
#>         Stem  Branches Leaves Needles Bark    Aerial     Total
#> 1  1731.1354 1448.8434     NA      NA   NA 3179.9788  5374.456
#> 2  1836.4987 1308.8274     NA      NA   NA 3145.3260  5473.367
#> 3  1558.4148  877.7623     NA      NA   NA 2436.1770  3995.804
#> 4  1129.1675  958.3115     NA      NA   NA 2087.4791  3621.113
#> 5  2456.6831 1541.9793     NA      NA   NA 3998.6624  6918.243
#> 6   970.2358  748.7747     NA      NA   NA 1719.0105  2872.062
#> 7  1274.1937  994.1445     NA      NA   NA 2268.3382  3782.620
#> 8   689.6046 1459.2078     NA      NA   NA 2148.8123  3605.777
#> 9  1956.3415 1003.3391     NA      NA   NA 2959.6805  4819.651
#> 10 2396.5183 1511.0556     NA      NA   NA 3907.5739  6439.200
#> 11 1731.9384  739.1812     NA      NA   NA 2471.1196  3902.964
#> 12 2576.8722 1613.8816     NA      NA   NA 4190.7537  6912.901
#> 13 1488.7735  902.8881     NA      NA   NA 2391.6617  3964.367
#> 14 1658.6352 1208.3449     NA      NA   NA 2866.9801  4969.552
#> 15 2243.5062 1128.6643     NA      NA   NA 3372.1704  5505.159
#> 16 1545.4009  871.3284     NA      NA   NA 2416.7293  3963.332
#> 17  823.2194  911.0634     NA      NA   NA 1734.2828  2938.385
#> 18 3527.5949 3599.0741     NA      NA   NA 7126.6691 12474.670
#> 19 2334.2050 1360.8376     NA      NA   NA 3695.0426  6031.063
#> 20 3399.2454 2990.3581     NA      NA   NA 6389.6035  9259.096
#> 21 2454.0362 1611.6606     NA      NA   NA 4065.6968  5948.960
#> 22 1860.1125  960.1772     NA      NA   NA 2820.2897  4588.771
#> 23 2925.2421 1637.5517     NA      NA   NA 4562.7938  6620.594
#> 24 2668.7709 1465.3556     NA      NA   NA 4134.1265  6011.509
#> 25 4089.2321 2251.6699     NA      NA   NA 6340.9019  9102.461
#> 26 1412.6365  920.8413     NA      NA   NA 2333.4778  3469.178
#> 27 1724.3837  736.0165     NA      NA   NA 2460.4002  3885.999
#> 28 1242.2436 1272.3318     NA      NA   NA 2514.5755  3748.278
#> 29 3744.6264 1239.3008     NA      NA   NA 4983.9272  7023.313
#> 30 2828.5556 1730.9308     NA      NA   NA 4559.4865  6635.784
#> 31 4622.6688 2680.3234     NA      NA   NA 7302.9922 10424.794
#> 32 2302.8518  911.9597     NA      NA   NA 3214.8115  5039.321
#> 33 1261.7995 2035.4316     NA      NA   NA 3297.2311  4935.932
#> 34  797.2321  675.2139     NA      NA   NA 1472.4460  2291.056
#> 35  307.8727  430.0885     NA      NA   NA  737.9612  1234.734
#> 36  382.5484  433.8413     NA      NA   NA  816.3898  1248.477
#> 37  829.8596  714.0509     NA      NA   NA 1543.9106  2396.023
#> 38  699.1446  638.2328     NA      NA   NA 1337.3774  2055.270
#> 39  268.0153 1893.8082     NA      NA   NA 2161.8235  2915.979
#> 40 1449.5129 1021.1228     NA      NA   NA 2470.6356  3289.245

# Groups the result by DBH clases
IFNbiomass(example_tree_data,
           DBHclasses = seq(0, 120, by=5))
#>   "ID" "Species" "Name" "SpeciesAllom" "NameAllom" "DBHclass"    Roots     Stem
#> 1   ID   Species   Name   SpeciesAllom   NameAllom   DBHclass 72052.15 75199.53
#>   Branches Leaves Needles Bark   Aerial    Total
#> 1 52437.78      0       0    0 127637.3 199689.5
```
