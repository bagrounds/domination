module Version where

data Version = Version Int Int Int

version :: Version
version = Version major minor patch where
  major = 0
  minor = 32
  patch = 1

