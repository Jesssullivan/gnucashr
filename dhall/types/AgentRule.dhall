-- Agent authorization rule definition
-- Each rule maps a financial operation to an authorization tier with limits

let AuthorizationLevel = ./AuthorizationLevel.dhall

let AgentRule =
      { name : Text
      , description : Text
      , authorization : AuthorizationLevel
      , max_amount_cents : Optional Natural
      , rate_limit_per_hour : Natural
      , enabled : Bool
      }

in  AgentRule
