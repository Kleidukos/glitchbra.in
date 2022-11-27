+++
title = "Servant APIs: My Personal How-to"
date = 2021-05-08T13:48:59+02:00
description = "Hécate's guide to making Servant-based APIs"
draft = true
categories = ["Tutorials"]
tags = ["servant", "haskell", "web"]
[[copyright]]
  owner = "Hécate's blog"
  date = "2021"
  license = "cc-by-nd-4.0"
+++

## On Servant

[Servant][servant-url] is a Haskell library providing a DSL to express web APIs in a type-safe manner. You describe your APIs in terms of Haskell
types (rather than terms), and it enables you to specify what kind of data the endpoints are supposed to take in and give back.

Relying on type aliases and type-level combinators, an API using Servant may look like this:

```haskell
-- API type definition as a type, with the "choice" (:<|>) operator
-- to offer multiple endpoints.
type UserAPI = "users"  :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac"  :> Get '[JSON] User

-- Implementation of said API
server :: Server UserAPI
server = pure usersHandler
     :<|> pure albertHandler
     :<|> pure isaacHandler

-- Handlers for each route declared
isaacHandler :: User
isaacHandler = User "Isaac Newton" 372 "isaac@newton.co.uk"
                (fromGregorian 1683 3 1)

albertHandler :: User
albertHandler = User "Albert Einstein" 136 "ae@mc2.org"
                (fromGregorian 1905 12 1)

usersHandler :: [User]
usersHandler = [isaac, albert]
```

_(taken from the [official Servant tutorial][tutorial])_

---

## Beyond the basics

This is a basic way to declare your APIs.
Now, let us explore a combination of features in Servant that I find utterly indispensible:

* Named Routes
* Custom Monad
* Contexts

### Named Routes

_Named Routes_ are an alternative way to specify Servant APIs through the use of a record of handlers, rather than a type-level tree of
combinators. Nesting sub-APIs in records of functions provides us with a cleaner way to organise our APIs, and more importantly: the ablity to name our routes:

```Haskell

type UserAPI = NamedRoutes UserAPI'

data UserAPI' mode = UserAPI'
  { users  :: mode :- "users"  :> Get '[JSON] [User]
  , albert :: mdoe :- "albert" :> Get '[JSON] User
  , isaac  :: mdoe :- "isaac"  :> Get '[JSON] User
  }
  deriving stock (Generic)

server :: Server UserAPI
server = UserAPI' 
        { users  = usersHandler
        , albert = albertHandler
        , isaac  = isaacHandler
        }

usersHandler  :: Server UserAPI 
isaacHandler  :: Server UserAPI 
albertHandler :: Server UserAPI 
```

### Custom Monad

A custom monad can be used to give the handlers access to some piece of information that they will inevitably use. I am talking here about some data taken
from the outside at start-up time, or maybe an [IORef][IORef] pointing to a mutable store that handles user sessions. The data stored here should help with the
business requirements of your API (storing user sessions' data, a connection pool, etc):

```haskell
type APIMonad = ReaderT AppState Handler
```

with AppState being declared as:

```haskell
newtype SessionId = Text

data SessionJar = TVar (HashMap SessionId Session) 

data Session =                      
  Session { sessionId  :: SessionId   
          , validUntil :: UTCTime
          , content    :: Maybe (HashMap Text Text)
          } deriving (Show, Eq)

data AppState
  = AppState { startupPort :: Word16
             , sessions :: SessionJar
             } deriving stock (Eq, Show)

```

The `APIMonad` is a type alias over `ReaderT`, so we can transparently use the `asks f` function to select
a field in the `AppState` record that the Reader holds:

```haskell
startupPortHandler :: APIMonad Text
startupPortHandler = do
  port <- asks startupPort
  pure $ "Connection port is: " <> display port
```

### Context

In Servant 0.5 was introduced the concept of `Context`. A `Context` is a way to pass values to the route combinators. The most popular usage of a
`Context` is to implement an authentication guard that is used next to route declarations in the API types. Basic Auth, JWT, OAuth are authentication
schemes that are best implemented through the use of a `Context` (*even if JWT is best not used at all*).


```Haskell
-- In API/Server.hs

module API.Server where

import Servant
import Servant.API.Generic
import Servant.Server.Generic

import qualified API.Server.User as User

-- | We declare our API endpoint as depending on another API "server".
data API mode = API
  { users :: mode :- "users" :> User.API
  } deriving stock Generic

runAPI :: AppState -> IO ()
runAPI state = 
  app params >>= runSettings warpSettings
  where warpSettings = setPort (v3Port params) defaultSettings

app :: AppState -> IO Application
app appState = pure $
  genericServeTWithContext
    (naturalTransform appState) -- The function that injects
                                -- the app state to our handlers
    server  -- Our routes
    (genAuthServerContext appState) -- The context generator

server :: API (AsServerT APIMonad)
server = API { user = User.server }

naturalTransform :: AppState -> APIMonad a -> Handler a
naturalTransform state v3App =
  runReaderT v3App state

-- | We will see this function later… ;)
genAuthServerContext :: AppState -> Context '[AuthHandler]
genAuthServerContext appState = authHandler appState :. EmptyContext
```


[servant-url]: https://docs.servant.dev/en/stable/
[tutorial]: https://docs.servant.dev/en/stable/tutorial/Server.html
[IORef]: https://hackage.haskell.org/package/base/docs/Data-IORef.html#t:IORef
