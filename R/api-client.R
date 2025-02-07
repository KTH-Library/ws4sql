#' Create a connection to a ws4sql server instance
#' @param connectionstring the connectionstring to use for the ws4sql server
#' @param quiet a boolean indicating whether some messages should be displayed
#' @param api_key a character only provided if using an API key
#' @details
#' The connectionstring should follow this pattern:
#' http(s)://user:pass@hostname/path/
#' 
#' Examples:
#' 
#' http://localhost/duckserve/
#' https://user:pass@some.server.tld:8000/duckserve/
#' 
#' When not using basic auth (like above), use the api_key parameter
#' 
#' @import httr2
#' @return a connection object which can be used with the other functions
#' @export
ws4sql_con <- function(connectionstring = NULL, quiet = TRUE, api_key) {

    if (is.null(connectionstring)) {
      connectionstring <- "http://localhost/duckserve/"
      if (!quiet) {
        message("No connectionstring (example: 'https://user:pass@some.server.tld/duckserve/') was given")
        message("Attempting default 'http://localhost/duckserve/")
      }
    }
  
    con <- httr2::url_parse(connectionstring)
  
    if (!is.null(con$username) & !is.null(con$password)) {
      user <- con$username
      con$username <- NULL
      pass <- con$password
      con$password <- NULL
      if (!quiet) message("Using basic authorized connection, user and pass was provided.")
    } else {
      user <- pass <- ""
      if (!quiet) message("Note: unauthenticated connection, no user/pass provided.")
    }
  
    if (!quiet) print(con)
  
    url_base <- httr2::url_build(con)
  
    con <- list(
      url_base = url_base,
      user = user,
      pass = pass, 
      quiet = quiet
    )
  
    if (!missing(api_key)) con$api_key <- api_key
    return(con)
  
  }
  
  
#' Check that a ws4sql instance is available
#' 
#' @param con the connection for the ws4sql, see ws4sql_con()
#' @import httr2
#' @return TRUE if available, otherwise FALSE
#' @export
ws4sql_ping <- function(con = ws4sql_con(quiet = TRUE)) {
    req <- request(con$url_base)
    if (!is.null(con$user) & !is.null(con$pass)) {
        req <- req |> 
            req_auth_basic(username = con$user, password = con$pass)
    }
    resp <- req |> req_error(is_error = ~ FALSE) |> req_perform() 
    if (resp_status(resp) %in% c(200, 405)) {
        if (resp_status(resp) == 405) 
            if (!(con$quiet)) message("API seems to be alive, requires authentication.")
        return(invisible(TRUE))
    }
    stop(resp_status_desc(resp))
}
  
#' Read data from ws4sql
#' @param con the connection, by default from duckhttp_con()
#' @param statement the web transaction to issue against the ws4sql httpserver
#' @return a tibble with results from the query
#' @import httr2
#' @import RcppSimdJson
#' @importFrom readr type_convert
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @export
ws4sql_read <- function(con = ws4sql_con(quiet = TRUE), statement = NULL) {

    if (is.null(statement))
        stop("Please provide a valid ws4sql statement")
    if (!ws4sql_ping(con))
        stop("No connection available")
    
    q <- list(transaction = list(list(query = statement)))

   q |> jsonlite::toJSON(auto_unbox = TRUE)
    # '{"transaction": [{"query": "#Q1" }]}' |> 
    # jsonlite::fromJSON(simplifyVector = FALSE) |> str()
    
    req <- 
        request(con$url_base) |>
        req_method("POST") |> 
        req_headers(`Content-Type` = "application/json") |> 
        req_body_json(q) 

    # authenticate if required
    if (!is.null(con$api_key)) {
        req <- 
            req |> req_headers(`X-API-Key` = con$api_key, .redact = "Authorization")
    } else if (!is.null(con$user) & !is.null(con$pass)) {
        req <- 
            req |> req_auth_basic(username = con$user, password = con$pass)
    }



    resp <- 
        req |> req_perform()

    resp |> resp_body_json()

    # TODO: parse respone into tibbles
    #resp |> tibble_from_jsoncompact()

}

ct <- function(types) {

    lookup <- function(x) {
        switch(x,
            "Float" = "d",
            "Double" = "d",
            "Int32" = "i",
            "Int64" = "i",
            "UInt64" = "i",
            "String" = "c",
            "DateTime" = "?",
            "Date" = "D",
            "Int8" = "l",
            "?"
        )
    }

    vapply(types, lookup, character(1)) |> unname() |> 
    paste(collapse = "")

}

tibble_from_jsoncompact <- function(resp) {
    obj <- httr2::resp_body_raw(resp) |> RcppSimdJson::fparse()
    df <- obj$data |> as.data.frame() |> tibble::as_tibble() |> setNames(nm = obj$meta$name)
    ct <- rep("?", ncol(df)) |> paste(collapse = "")
    df |> readr::type_convert(guess_integer = TRUE, col_types = ct(obj$meta$type)) 
}
