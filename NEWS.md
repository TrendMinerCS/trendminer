# trendminer (development version)

## New functions

### Authentication 

* `tm_token()` fetches a Bearer access token from TrendMiner which is used for any subsequent API requests.

### Asset functions

* `tm_search_assets()` searches for assets and tags in the asset framework based on a query.

* `tm_assets()` gets the complete list of available assets.

* `tm_tags()` gets the complete list of available tags.

* `tm_root_structures()` gets all root structures.

* `tm_child_structures()` gets all child structures of a structure.

* `tm_descendant_structures()` gets all descendants (the entire structure subtree) of a structure.

## Documentation

* New introduction vignette (see `vignette("trendminer")`) to help you get up and running with trendminer.
