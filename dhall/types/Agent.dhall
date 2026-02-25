-- Agent persona configuration type
-- Defines what tools an agent can access and its behavior

let AuthorizationLevel = ./AuthorizationLevel.dhall

let Schedule =
      -- Agent scheduling options
      < Hourly : Natural       -- Every N hours
      | Daily : Natural        -- Every N days at midnight UTC
      | OnDemand               -- Manual triggering only
      >

let VendorPattern =
      -- Transaction categorization pattern
      { Type =
          { pattern : Text              -- Regex or substring match
          , category : Text             -- Target account path
          , confidence : Double         -- 0.0-1.0 (how confident is this match)
          }
      , default =
          { confidence = 0.95 }
      }

let Agent =
      { Type =
          { name : Text                           -- Agent name (e.g., "spend-monitor")
          , description : Text                    -- Human-readable description
          , tools : List Text                     -- MCP tool names this agent can use
          , authorization_level : AuthorizationLevel
          , vendor_patterns : List VendorPattern.Type
          , schedule : Optional Schedule
          }
      , default =
          { schedule = None Schedule
          , authorization_level = AuthorizationLevel.Auto
          , vendor_patterns = [] : List VendorPattern.Type
          }
      }

in  { Agent, Schedule, VendorPattern }
