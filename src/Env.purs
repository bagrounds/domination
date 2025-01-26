--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Empty environment type definition.
--|
--| ### Key Concepts
--| * Type
--| * Environment (data structure)
--| * Empty data type
module Domination.Env where

type Env =
  { wsUrl :: String
  }

env :: Env
env =
  { wsUrl: "ws://localhost:8080"
  }
