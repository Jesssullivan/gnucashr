-- Global rate limits and circuit breaker configuration
-- Applied across all agents regardless of individual rule limits

let RateLimitConfig =
      { global_max_writes_per_hour : Natural
      , global_max_reads_per_hour : Natural
      , circuit_breaker_threshold : Natural
      , circuit_breaker_window_seconds : Natural
      , cooldown_seconds : Natural
      , max_pending_approvals : Natural
      }

let defaults
    : RateLimitConfig
    = { global_max_writes_per_hour = 50
      , global_max_reads_per_hour = 500
      , circuit_breaker_threshold = 10
      , circuit_breaker_window_seconds = 60
      , cooldown_seconds = 300
      , max_pending_approvals = 25
      }

in  { RateLimitConfig, defaults }
