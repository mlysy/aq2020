# AQ2020: Software to Replicate Analysis of Paper by Al-Abadleh et al (2021)

*Martin Lysy, Hind A. Al-Abadleh, Lucas Neil, Priyesh Patel, Wisam Mohammed, Yara Khalaf*

The paper can be found [here](https://www.sciencedirect.com/science/article/pii/S0304389421004088).

---

## Installation

Install the R package [**devtools**](https://CRAN.R-project.org/package=devtools) and run
```r
devtools::install_github("mlysy/aq2020")
```

## Usage

Please see the following scripts in `inst/extdata`:

- [`aq2020-pollutant_extdata.R`](inst/extdata/aq2020-pollutant_extdata.R) and [`aq2020-pollutant_pvalue.R`](inst/extdata/aq2020-pollutant_pvalue.R): Preprocessing and p-value calculations for the pollutant data.

- [`aq2020-temperature_extdata.R`](inst/extdata/aq2020-temperature_extdata.R) and [`aq2020-temperature_pvalue.R`](inst/extdata/aq2020-temperature_pvalue.R): Preprocessing and p-value calculations for the temperature data.

- [`aq2020-irradiance_extdata.R`](inst/extdata/aq2020-irradiance_extdata.R) and [`aq2020-irradiance_pvalue.R`](inst/extdata/aq2020-irradiance_pvalue.R): Preprocessing and p-value calculations for the solar irradiance data.

- [`aq2020-pval_plots.R`](inst/extdata/aq2020-pval_plots.R): P-value boxplots in the paper.

## TODO

- [ ] Turn scripts into vignettes.

- [ ] Standardize capitalization of columns in datasets.

- [ ] Replace `poll_*` by `pollutant_*`.

- [ ] Document datasets.

- [ ] Add 3-day webscrape function `get_aqo3d()`.  What's missing is to format the output in `H00:H23` format (always EST).

- [ ] Use `get_aqo3d()` to fill in November-December 2020 for `Downtown_Toronto`, after the station changed locations.  Should store the new `Station_ID` though.  

- [ ] Filter `pollutant_info` on `has_poll`.

- [ ] Perhaps `pollutant_info` and `temperature_info` don't need to be stored in `data`, but rather only in `inst/ext_data`.  That is, could easily recover the relevant information from `pollutant_data` and `temperature_data`, especially since we want to add `station_id` as identifier now, since `station` can have more than one of these.

