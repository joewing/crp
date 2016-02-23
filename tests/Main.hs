
import Test.QuickCheck
import CRP(
        logGamma
    )

main :: IO ()
main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests

prop_logGamma d = logGamma d >= 0.0

tests = [
    ("logGamma", quickCheck prop_logGamma)]
