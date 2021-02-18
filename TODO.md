# TODO

- [ ] Standardize capitalization of columns in datasets.

- [ ] Replace `poll_*` by `pollutant_*`.

- [ ] Document datasets.

- [ ] Add 3-day webscrape function `get_aqo3d()`.  What's missing is to format the output in `H00:H23` format (always EST).

- [ ] Use `get_aqo3d()` to fill in November-December 2020 for `Downtown_Toronto`, after the station changed locations.  Should store the new `Station_ID` though.  

- [ ] Filter `pollutant_info` on `has_poll`.

- [ ] Perhaps `pollutant_info` and `temperature_info` don't need to be stored in `data`, but rather only in `inst/ext_data`.  That is, could easily recover the relevant information from `pollutant_data` and `temperature_data`, especially since we want to add `station_id` as identifier now, since `station` can have more than one of these.

