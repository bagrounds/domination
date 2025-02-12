--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a version data type with three integer components (major, minor, patch).
--|
--| ### Key Concepts
--| * Data types
--| * Module definition
--| * Pattern matching

module Version where

data Version = Version Int Int Int

version :: Version
version = Version major minor patch where
  major = 0
  minor = 33
  patch = 9
