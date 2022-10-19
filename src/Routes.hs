module Routes
  ( routes,
  )
where

import User (userRouter)
import Web.Scotty
import Types (Config)

routes :: Config -> ScottyM ()
routes config = do
  userRouter config
  testRoute
  echoRoute
  notFoundRouter

testRoute :: ScottyM ()
testRoute = get "/running" $ text "yes"

echoRoute :: ScottyM ()
echoRoute = get "/echo/:text" $ do
  res <- param "text"
  text $ "echo: [" <> res <> "]"

notFoundRouter :: ScottyM ()
notFoundRouter = notFound $ text "leave..."
