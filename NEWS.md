# refloraR 1.0.0

## Initial Release

The first official release of the `refloraR` R package, designed to streamline access to plant specimen data from the REFLORA Virtual Herbarium hosted by the Rio de Janeiro Botanical Garden.

### Features

- `reflora_summary()`: Retrieve metadata and summary info from all or specific REFLORA herbaria.
- `reflora_download()`: Download original specimen data in Darwin Core Archive (DwC-A) format.
- `reflora_records()`: Parse, filter, and organize REFLORA records based on taxon, herbarium, region, and year.
- `reflora_indets()`: Retrieve indeterminate specimens (e.g., identified only to family or genus rank).
- Optional filters by `taxon`, `herbarium`, `state`, `recordYear`, and `level`.
- Supports integration with tidyverse workflows for downstream analyses.
- Repatriated collection filtering (`repatriated = TRUE` by default).
- Test coverage >95%, continuous integration via GitHub Actions.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check and test coverage.
- Hosted documentation: [refloraR-website](https://dboslab.github.io/refloraR-website/)

### Feedback

Please report bugs or issues at:  
<https://github.com/DBOSlab/refloraR/issues>
