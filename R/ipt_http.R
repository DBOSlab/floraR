# Shared HTTP headers for the JBRJ IPT metadata and archive requests.
.ipt_http_headers <- function() {
  c(`User-Agent` = "Mozilla/5.0 (compatible; floraR/1; Linux)")
}
