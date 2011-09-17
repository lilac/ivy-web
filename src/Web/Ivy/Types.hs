{-# LANGUAGE OverloadedStrings #-}
module Web.Ivy.Types 
    (Handler(..)
    , Application)
    where
import Network.Wai
import Network.HTTP.Types

class Handler a where
    get, post, put, delete, handle :: a -> Application
    get _ = unimplemented
    post _ = unimplemented
    put _ = unimplemented
    delete _ = unimplemented

    handle a req = case requestMethod req of
        m | m == methodGet -> get a req
          | m == methodPost -> post a req
          | m == methodPut -> put a req
          | m == methodDelete -> delete a req
        otherwise -> unimplemented req

unimplemented :: Application
unimplemented _ = return $ responseLBS status501 [("Content-Type", "text/plain")] "not implemented method"
