{-|
Module for defining and managing the version of the application.

This module provides a data type for representing version numbers and
a value that holds the current version of the application.

Key Components:
- Version: Data type representing a version number with major, minor, and patch components
- version: The current version of the application

Usage:
Import this module to access the current version of the application.
-}

module Version where

data Version = Version Int Int Int

version :: Version
version = Version major minor patch where
  major = 0
  minor = 33
  patch = 3

