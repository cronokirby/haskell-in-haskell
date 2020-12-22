
module CWriter (writeC) where

import Ourlude
import Cmm

-- | A type for CCode.
--
-- We could use something more efficient than a string, but this is ok
-- for our explanation purposes
type CCode = String

writeC :: Cmm -> CCode
writeC _ = "int main() { return 0; }"
