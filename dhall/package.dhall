-- gnucashr Dhall configuration package
-- Entry point for all typed configuration
--
-- Usage:
--   dhall type --file dhall/package.dhall
--   dhall-to-json --file dhall/rules/authorization.dhall

{ types = ./types.dhall
, rules =
  { authorization = ./rules/authorization.dhall
  , rateLimits = ./rules/rate-limits.dhall
  }
, templates =
  { saasSpend = ./templates/saas-spend.dhall
  , taxCategories = ./templates/tax-categories.dhall
  , incomeCategories = ./templates/income-categories.dhall
  }
}
