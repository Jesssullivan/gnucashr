-- Agent authorization tiers for financial operations
--
-- Auto:    Read-only operations, reports, categorization
-- Review:  Creates artifacts requiring human review before finalization
-- Approve: Moves money or modifies account structure; requires explicit approval

let AuthorizationLevel = < Auto | Review | Approve >

in  AuthorizationLevel
