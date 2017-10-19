# FRED
This repo contains code for downloading and parsing data from the FRED API. API documentation is [here](https://research.stlouisfed.org/docs/api/).
Using the API requires a key, which you can request [here](https://research.stlouisfed.org/docs/api/api_key.html).
As of now, there are two main files in this repo:
`fred_api.R` contains functions  for working with the FRED API.
`recession_shading.R` contains a function for adding recession shading to plots (useful beyond the FRED API).
I hope to add code for working with GeoFRED, and improved functionality for ALFRED in the future.