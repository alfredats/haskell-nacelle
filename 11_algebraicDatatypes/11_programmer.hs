module Programmer where

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac 
                     | Windows
                     deriving (Eq, Show)


data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq,Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang }
                             deriving (Eq, Show)


nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda 
                             , os = GnuPlusLinux }


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows ]

allLanguages :: [ProgLang] 
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems
                                    , lang <- allLanguages ]

