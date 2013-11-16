import Distribution.Admiral.Error
import Distribution.Admiral.Options
import Distribution.Admiral.Operation

main :: IO ()
main = execParser (info (helper <*> commands) fullDesc) >>= safely . runOperation
