--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a simple version number data type with pre-defined values for major, minor, and patch numbers.
--|
--| ### Key Concepts
--| * Data types and pattern matching
--| * Defining a data type with associated values
--| * Pattern binding in a data definition

module Version where

data Version = Version Int Int Int

version :: Version
version = Version major minor patch where
  major = 0
  minor = 33
  patch = 3
