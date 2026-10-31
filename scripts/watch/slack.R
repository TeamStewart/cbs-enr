# ========================================
## Watcher: Slack Web API client. With no SLACK_BOT_TOKEN every call is appended to
## state/slack_outbox.log and returns a fake ts, so the whole watcher runs in dry-run.
# ========================================

SLACK_TOKEN = Sys.getenv("SLACK_BOT_TOKEN")
SLACK_CHANNEL = Sys.getenv("SLACK_CHANNEL")
SLACK_DRY = !nzchar(SLACK_TOKEN) || !nzchar(SLACK_CHANNEL)
APPROVERS = local({
  a = trimws(strsplit(Sys.getenv("CBS_WATCH_APPROVERS"), ",")[[1]])
  a[nzchar(a)]
})

slack_call = function(method, body = list(), http = "POST") {
  if (SLACK_DRY) {
    cat(fmt_time(), method, toJSON(body, auto_unbox = TRUE), "\n", file = state_path("slack_outbox.log"), append = TRUE)
    # rehearsing without Slack: messages in state/dry_inbox.json ([{user, text, thread_ts}])
    # are delivered once, at the top level
    if (method == "conversations.history") {
      f = state_path("dry_inbox.json")
      msgs = read_json_file(f, list())
      unlink(f)
      for (i in seq_along(msgs)) msgs[[i]]$ts = msgs[[i]]$ts %||% sprintf("%.6f", as.numeric(Sys.time()) + i / 1000)
      return(list(ok = TRUE, messages = msgs))
    }
    if (method == "conversations.replies") return(list(ok = TRUE, messages = list()))
    return(list(ok = TRUE, ts = sprintf("%.6f", as.numeric(Sys.time())), dry = TRUE))
  }
  req = httr2::request(paste0("https://slack.com/api/", method)) |>
    httr2::req_auth_bearer_token(SLACK_TOKEN) |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 3, is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 502, 503))
  req = if (http == "GET") httr2::req_url_query(req, !!!body) else httr2::req_body_json(req, body)
  out = tryCatch(httr2::resp_body_json(httr2::req_perform(req)), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  if (!isTRUE(out$ok)) message("slack ", method, " failed: ", out$error %||% "?")
  out
}

slack_post = function(text, thread_ts = NULL, broadcast = FALSE) {
  body = list(channel = SLACK_CHANNEL, text = text, unfurl_links = FALSE, unfurl_media = FALSE)
  if (!is.null(thread_ts)) body$thread_ts = thread_ts
  if (broadcast) body$reply_broadcast = TRUE
  r = slack_call("chat.postMessage", body)
  if (isTRUE(r$ok)) r$ts else NULL
}

slack_update = function(ts, text) isTRUE(slack_call("chat.update", list(channel = SLACK_CHANNEL, ts = ts, text = text))$ok)
slack_pin = function(ts) isTRUE(slack_call("pins.add", list(channel = SLACK_CHANNEL, timestamp = ts))$ok)

# messages newer than `oldest` in a thread (thread_ts) or at the channel's top level
slack_read = function(oldest, thread_ts = NULL) {
  q = list(channel = SLACK_CHANNEL, oldest = oldest, limit = 100)
  r = if (is.null(thread_ts)) slack_call("conversations.history", q, http = "GET")
      else slack_call("conversations.replies", c(q, ts = thread_ts), http = "GET")
  msgs = r$messages %||% list()
  # replies include the root; keep only messages after `oldest`, from people (no bots)
  Filter(function(m) as.numeric(m$ts) > as.numeric(oldest) && is.null(m$bot_id) && !is.null(m$user), msgs)
}

slack_auth_ok = function() SLACK_DRY || isTRUE(slack_call("auth.test")$ok)
