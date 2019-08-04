
#' Title
#'
#' @param region
#' @param verb
#' @param operation
#' @param body
#'
#' @return
#' @export
#'
#' @examples
ecs_http <- function(region, verb, operation, body) {
	creds <- aws.signature::locate_credentials(region = region)

	body_json <- jsonlite::toJSON(body)

	host <- paste0("ecs.", creds$region, ".amazonaws.com")
	date <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
	headers <- list(
		"X-Amz-Target" = paste0("AmazonEC2ContainerServiceV20141113.", operation),
		"X-Amz-Date" = date,
		"Content-Type" = "application/x-amz-json-1.1"
	)

	sig <- aws.signature::signature_v4_auth(
		datetime = date,
		region = creds$region,
		service = "ecs",
		verb = verb,
		action = "/",
		canonical_headers = list("Host" = host, "X-Amz-Date" = date),
		request_body = body_json,
		key = creds$key,
		secret = creds$secret,
		session_token = creds$session_token
	)

	headers[["Authorization"]] <- sig$SignatureHeader

	client <- crul::HttpClient$new(url = paste0("https://", host), headers = headers)
	res <- client$verb(verb, path = "/", body = body_json)

	if (res$status_code != 200) {
		status <- res$status_http()
		stop(paste0(operation, " failed, ", status$message, "(", status$status_code, "): ", res$parse()))
	}

	jsonlite::fromJSON(res$parse(encoding = "UTF-8"), flatten = T)
}
