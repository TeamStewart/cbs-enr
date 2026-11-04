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

# chat.postMessage allows about 1 post per second per channel; bursts get 429s
SLACK_GAP = 1.1
SLACK_CHUNK = 3500 # characters per combined message
SLACK_OUT = new.env()
SLACK_OUT$last = 0

slack_post = function(text, thread_ts = NULL, broadcast = FALSE) {
  if (!SLACK_DRY) {
    wait = SLACK_OUT$last + SLACK_GAP - as.numeric(Sys.time())
    if (wait > 0) Sys.sleep(wait)
    on.exit(SLACK_OUT$last <- as.numeric(Sys.time()))
  }
  body = list(channel = SLACK_CHANNEL, text = text, unfurl_links = FALSE, unfurl_media = FALSE)
  if (!is.null(thread_ts)) body$thread_ts = thread_ts
  if (broadcast) body$reply_broadcast = TRUE
  r = slack_call("chat.postMessage", body)
  if (isTRUE(r$ok)) r$ts else NULL
}

# ---- per-tick outbox: tick.R queues, then slack_flush() sends combined messages ----
# roots: new incident posts, keyed by incident id (all of a tick's roots go in one message,
# so those incidents share one thread; verbs name the incident, so replies still route).
# msgs: list(text, thread), thread = NULL (top level), an incident id, or a Slack ts.
slack_reset = function() {
  SLACK_OUT$roots = list()
  SLACK_OUT$msgs = list()
}
slack_reset()

slack_queue_root = function(inc) SLACK_OUT$roots[[inc$id]] = incident_text(inc)
slack_queue = function(text, thread = NULL) SLACK_OUT$msgs[[length(SLACK_OUT$msgs) + 1]] = list(text = text, thread = thread)

# join texts with blank lines into chunks of at most SLACK_CHUNK characters
slack_chunks = function(texts) {
  out = list()
  cur = integer()
  for (i in seq_along(texts)) {
    if (length(cur) && sum(nchar(texts[c(cur, i)]) + 2) > SLACK_CHUNK) {
      out[[length(out) + 1]] = cur
      cur = integer()
    }
    cur = c(cur, i)
  }
  if (length(cur)) out[[length(out) + 1]] = cur
  out
}

# send everything queued this tick; returns db with slack_ts set on newly posted incidents
slack_flush = function(db) {
  roots = SLACK_OUT$roots
  roots = roots[vapply(names(roots), function(id) !is.null(db$incidents[[id]]) && is.null(db$incidents[[id]]$slack_ts), TRUE)]
  if (length(roots)) {
    txt = unlist(roots)
    for (ch in slack_chunks(txt)) {
      head = if (length(roots) > 1) sprintf("*%d new incidents* (reply `approve/deny/ignore I-n` in this thread)\n\n", length(roots)) else ""
      ts = slack_post(paste0(head, paste(txt[ch], collapse = "\n\n")))
      if (!is.null(ts)) for (id in names(roots)[ch]) db$incidents[[id]]$slack_ts = ts
    }
  }
  # resolve threads to a ts, then group by thread, keeping first-seen order
  where = vapply(SLACK_OUT$msgs, function(m) {
    t = m$thread
    if (is.null(t)) return("")
    if (!is.null(db$incidents[[t]])) return(db$incidents[[t]]$slack_ts %||% NA_character_)
    t
  }, "")
  keep = !is.na(where)
  if (any(!keep)) message("slack: dropped ", sum(!keep), " replies to incidents with no thread")
  msgs = vapply(SLACK_OUT$msgs, `[[`, "", "text")[keep]
  where = where[keep]
  for (w in unique(where)) {
    txt = msgs[where == w]
    for (ch in slack_chunks(txt)) slack_post(paste(txt[ch], collapse = "\n\n"), thread_ts = if (nzchar(w)) w else NULL)
  }
  slack_reset()
  db
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
