#' Plaid Link Helpers
#'
#' One-time OAuth flow for connecting a bank account via Plaid Link.
#' The link flow opens a browser for the user to authenticate with their
#' bank, then exchanges the resulting public token for a persistent
#' access token.
#'
#' @name plaid-link
#' @seealso [plaid_create_client()], [plaid_sync_transactions()]
NULL


#' Create Plaid Link Token
#'
#' Generate a Link token for initiating the Plaid Link flow.
#' The token is short-lived (30 minutes) and single-use.
#'
#' @param client A \code{plaid_client} from \code{plaid_create_client()}.
#' @param user_id Unique identifier for the user (any stable string).
#' @param products Character vector of Plaid products to enable
#'   (default \code{c("transactions")}).
#' @param country_codes Character vector of country codes
#'   (default \code{c("US")}).
#' @param language Link UI language (default "en").
#' @return Named list with \code{link_token} and \code{expiration}.
#' @export
#' @examples
#' \dontrun{
#' client <- plaid_create_client()
#' link <- plaid_create_link_token(client, user_id = "user-123")
#' cat("Link token:", link$link_token, "\n")
#' }
plaid_create_link_token <- function(client,
                                     user_id,
                                     products = c("transactions"),
                                     country_codes = c("US"),
                                     language = "en") {
  stopifnot(inherits(client, "plaid_client"))

  resp <- .plaid_post(client, "/link/token/create", list(
    client_id = client$client_id,
    secret = client$secret,
    user = list(client_user_id = user_id),
    client_name = "gnucashr",
    products = as.list(products),
    country_codes = as.list(country_codes),
    language = language
  ))

  list(
    link_token = resp$link_token,
    expiration = resp$expiration
  )
}


#' Exchange Public Token for Access Token
#'
#' After the user completes the Plaid Link flow, exchange the
#' short-lived public token for a persistent access token.
#'
#' @param client A \code{plaid_client}.
#' @param public_token The public token received from Plaid Link.
#' @return Named list with \code{access_token} and \code{item_id}.
#' @export
#' @examples
#' \dontrun{
#' result <- plaid_exchange_token(client, "public-sandbox-abc123")
#' access <- result$access_token
#' }
plaid_exchange_token <- function(client, public_token) {
  stopifnot(inherits(client, "plaid_client"))

  resp <- .plaid_post(client, "/item/public_token/exchange", list(
    client_id = client$client_id,
    secret = client$secret,
    public_token = public_token
  ))

  list(
    access_tok = resp$access_token,
    item_id = resp$item_id
  )
}


#' Run Plaid Link Server
#'
#' Start a minimal local HTTP server that hosts a Plaid Link page,
#' captures the public token after the user authenticates, and exchanges
#' it for an access token. The server opens the browser automatically
#' and shuts down after token exchange.
#'
#' Requires the \pkg{httpuv} package.
#'
#' @param client A \code{plaid_client}.
#' @param user_id User identifier for the link token.
#' @param port Local port for the server (default 1410).
#' @param browse Whether to open the browser automatically (default TRUE).
#' @return Named list with \code{access_token} and \code{item_id},
#'   or NULL if the user cancels.
#' @export
#' @examples
#' \dontrun{
#' client <- plaid_create_client(environment = "sandbox")
#' result <- plaid_link_server(client, user_id = "jess")
#' if (!is.null(result)) {
#'   cat("Access token obtained! Store it securely.\n")
#' }
#' }
plaid_link_server <- function(client,
                               user_id = "gnucashr-user",
                               port = 1410L,
                               browse = TRUE) {
  rlang::check_installed("httpuv", reason = "for the Plaid Link local server")
  stopifnot(inherits(client, "plaid_client"))

  # Create link token
  link_info <- plaid_create_link_token(client, user_id = user_id)

  # State: captured token
  result_env <- new.env(parent = emptyenv())
  result_env$done <- FALSE
  result_env$public_token <- NULL

  # HTML page with embedded Plaid Link
  html_page <- .plaid_link_html(link_info$link_token, port)

  app <- list(
    call = function(req) {
      path <- req$PATH_INFO

      if (path == "/" || path == "") {
        # Serve the Link page
        return(list(
          status = 200L,
          headers = list("Content-Type" = "text/html"),
          body = html_page
        ))
      }

      if (path == "/callback" && req$REQUEST_METHOD == "POST") {
        # Read POST body
        body <- rawToChar(req$rook.input$read())
        parsed <- jsonlite::fromJSON(body)
        result_env$public_token <- parsed$public_token
        result_env$done <- TRUE
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(status = "ok"), auto_unbox = TRUE)
        ))
      }

      if (path == "/cancel") {
        result_env$done <- TRUE
        return(list(
          status = 200L,
          headers = list("Content-Type" = "text/html"),
          body = "<html><body><p>Link canceled. You may close this tab.</p></body></html>"
        ))
      }

      list(status = 404L, headers = list(), body = "Not found")
    }
  )

  server <- httpuv::startServer("127.0.0.1", port, app)
  on.exit(httpuv::stopServer(server), add = TRUE)

  url <- paste0("http://127.0.0.1:", port)
  message("Plaid Link server running at ", url)

  if (browse) {
    utils::browseURL(url)
  }

  # Service requests until done or timeout (5 minutes)
  deadline <- Sys.time() + 300
  while (!result_env$done && Sys.time() < deadline) {
    httpuv::service(100)
    Sys.sleep(0.1)
  }

  if (is.null(result_env$public_token)) {
    message("Plaid Link was canceled or timed out.")
    return(invisible(NULL))
  }

  # Exchange for access token
  message("Exchanging public token for access token...")
  result <- plaid_exchange_token(client, result_env$public_token)
  message("Success! Access token obtained for item: ", result$item_id)

  invisible(result)
}


#' Generate Plaid Link HTML Page
#'
#' @param link_token Plaid Link token.
#' @param port Callback port.
#' @return HTML string.
#' @keywords internal
.plaid_link_html <- function(link_token, port) {
  paste0('<!DOCTYPE html>
<html>
<head>
  <title>gnucashr - Connect Your Bank</title>
  <script src="https://cdn.plaid.com/link/v2/stable/link-initialize.js"></script>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 600px; margin: 50px auto; padding: 20px; }
    h1 { color: #333; }
    button { padding: 12px 24px; font-size: 16px; background: #0052ff; color: white;
             border: none; border-radius: 6px; cursor: pointer; }
    button:hover { background: #003fcc; }
    #status { margin-top: 20px; color: #666; }
  </style>
</head>
<body>
  <h1>gnucashr - Bank Connection</h1>
  <p>Click the button below to securely connect your bank account via Plaid.</p>
  <button id="link-btn" onclick="openLink()">Connect Bank Account</button>
  <div id="status"></div>
  <script>
    const handler = Plaid.create({
      token: "', link_token, '",
      onSuccess: function(public_token, metadata) {
        document.getElementById("status").innerText = "Exchanging token...";
        fetch("http://127.0.0.1:', port, '/callback", {
          method: "POST",
          headers: {"Content-Type": "application/json"},
          body: JSON.stringify({public_token: public_token})
        }).then(function() {
          document.getElementById("status").innerText =
            "Connected! You may close this tab.";
          document.getElementById("link-btn").disabled = true;
        });
      },
      onExit: function(err) {
        if (err) {
          document.getElementById("status").innerText = "Error: " + err.display_message;
        }
      }
    });
    function openLink() { handler.open(); }
  </script>
</body>
</html>')
}
