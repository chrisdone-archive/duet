-----------------------------------------------------------------------------
-- Test Framework:
import SourcePrelude
import Testbed
import HaskellPrims


main         :: IO ()
main          = test static
                     defnsHaskellPrims
                     (preludeDefns ++ hugsSpecific ++ preludeMems)
