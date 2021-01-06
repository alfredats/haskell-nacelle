module SemVer where

import Text.Trifecta


data NumberOrString = NOSS String
                    | NOSI Integer

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata


parseSemVer :: Parser SemVer
parseSemVer = undefined


