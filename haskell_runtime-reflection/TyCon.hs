module TyCon where

import           Type
-- we simulate GHC here, manually generate TyCon for some common types.
-- You do need a full signature of a type, because it's possible that types in
-- different modules have overlapping names.

tcInt :: TyCon'
tcInt = TyCon' { tcModule = Module'
                   { modPkg = "base"
                   , modName = "Data.Int"
                   }
               , tcName = "Int"
               }

tcChar :: TyCon'
tcChar = TyCon' { tcModule = Module'
                   { modPkg = "base"
                   , modName = "Data.Char"
                   }
               , tcName = "Char"
               }


tcMaybe :: TyCon'
tcMaybe = TyCon' { tcModule = Module'
                   { modPkg = "base"
                   , modName = "Data.Maybe"
                   }
               , tcName = "Maybe"
               }


tcList :: TyCon'
tcList = TyCon' { tcModule = Module'
                   { modPkg = "base"
                   , modName = "Data.List"
                   }
               , tcName = "List"
               }

-- GHC will generate a TyCon for every new type declaration. These TyCon are
-- also used for template haskell to splice a type.
