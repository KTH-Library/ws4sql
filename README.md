
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ws4sql

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/KTH-Library/ws4sql/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/ws4sql/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ws4sql is to provide an R client against the `ws4sql` REST
API which serves JSON over HTTP.

``` r
library(ws4sql)

# below we assume a ws4sql server is running locally 
# serving http requests against http://localhost:8000/duckserve/
# the url_base parameter can be used to specify any other location of the service

con <- ws4sql_con("http://ducky:duckz@localhost:8000/duckserve", quiet = FALSE)
#> Using basic authorized connection, user and pass was provided.
#> <httr2_url> http://localhost:8000/duckserve
#> • scheme: http
#> • hostname: localhost
#> • port: 8000
#> • path: /duckserve

# to check that the service is up, use:
con |> ws4sql_ping()
#> API seems to be alive, requires authentication.

# to perform a query against ws4sql (perform a web transaction named "#Q1"), use:
con$quiet <- TRUE

# NB: TODO: fix rectangularization, for now just ...
ws4sql_read(con, statement = "#Q1") |> 
  getElement("results") |> 
  purrr::map_dfr("resultSet") |> tidyr::unnest(a)
#> # A tibble: 10 × 2
#>        a b         
#>    <int> <chr>     
#>  1     0 one-to-ten
#>  2     1 one-to-ten
#>  3     2 one-to-ten
#>  4     3 one-to-ten
#>  5     4 one-to-ten
#>  6     5 one-to-ten
#>  7     6 one-to-ten
#>  8     7 one-to-ten
#>  9     8 one-to-ten
#> 10     9 one-to-ten
```

To understand what kind of web transactions are possible, see a small
example below in the `duck.yaml` configuration file.

# Running ws4sql server backed by a duckdb database

Usage example with duckdb.

## Installation

Get the software, download and unpack and make executable:

    # download
    cd ~/bin
    wget -O ws4sql.tgz \
            https://github.com/proofrock/ws4sqlite/releases/download/v0.17dev5/ws4sql-v0.17dev5-linux-amd64.tar.gz
    
    # install
    gunzip ws4sql.tgz && \
            tar xvf ws4sql.tar && \
            rm ws4sql.tar && \
            chmod +x ws4sql
    
    # verify version
     ws4sql -version

Or use podman/docker:

    docker pull ghcr.io/proofrock/ws4sql:latest

As of writing, the latest version is tagged like this:

    docker pull ghcr.io/proofrock/ws4sql:v0.17dev5

Next configure the API - which is defined in a `server.yaml` file.

## Starting a ws4sql server using a yaml file and duckdb

A yaml file that serves an API backed by duckdb can look like this:

``` yaml
database:
  type: DUCKDB
  inMemory: true
  id: duckserve
#  disableWALMode: true
  readOnly: false

auth:
  mode: HTTP
  byCredentials:
    - user: ducky
      password: duckz
readOnly: false
#corsOrigin: https://myownsite.com

storedStatements:
  - id: Q0
    sql: |
      select 42
  - id: Q1
    sql: |
      select range(10) as a, 'one-to-ten' as b
  - id: Q2
    sql: >
      from read_csv_auto('https://csvbase.com/kinder/list-of-user-agents')
  - id: Q3
    sql: |
      from read_json_auto('https://api.openalex.org/works/W4388315306')
  - id: Q4
    sql: |
      install tpch; load tpch; call tpch_queries()

initStatements:
  - load json
```

Once this file is available and names ‘duck.yaml’, use it along with
this command:

    docker run --rm \
      -p "8000:8000" \
      -v $(pwd)/duck.yaml:/tmp/duck.yaml \
      ghcr.io/proofrock/ws4sql:latest \
              -port 8000 -db /tmp/duck.yaml

The command starts the server, mounts the configuration file into the
containers file system and starts the server with the ‘duck.yaml’
configuration.

Once running make sure requests work, using for example:

    curl -s -X POST \
    --user ducky:duckz \
    --header 'Content-Type: application/json' \
    --data '{"transaction": [{"query": "#Q1" }]}' \
    http://localhost:8000/duckserve
