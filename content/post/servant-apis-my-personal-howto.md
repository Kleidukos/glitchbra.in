+++
title = "Servant APIs: My Personal How-to"
date = 2021-05-08T13:48:59+02:00
description = "Hécate's guide to making Servant-based APIs"
draft = true
[[copyright]]
  owner = "Hécate's blog"
  date = "2021"
  license = "cc-by-nd-4.0"
+++

## On Servant

[Servant][servant-url] is a Haskell library providing a DSL to express web APIs in a type-safe manner. You describe your APIs in terms of Haskell
types (rather than terms), and it enables you to specify what kind of data the endpoints are supposed to take in and give back.

Heavily relying on type aliases and type-level combinators, an API using Servant may look like this:

```haskell
-- API type definition as a tree
type UserAPI = "users"  :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac"  :> Get '[JSON] User

-- Type-safe implementation of said API
server :: Server UserAPI
server = return users
     :<|> return albert
     :<|> return isaac

-- Handlers for each route declared
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk"
                (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"
                (fromGregorian 1905 12 1)

users :: [User]
users = [isaac, albert]
```

_(taken from the [official Servant tutorial][tutorial])_

---

## Beyond the basics

This is a very basic way to declare your APIs, and I think that while the tutorial shows the basics of the library quite well, it is a bit lightweight on a
combination of feature in Servant that I find utterly indispensible:

* Generics
* Custom Monad
* Contexts

Generics are an alternative way to specify Servant APIs through the use of a record of handlers, rather than a type-level tree of
combinators. Nesting sub-APIs in records of functions provides us with a cleaner way to organise our APIs. 

A custom monad can be used to give the handlers access to some piece of information that they will inevitably use. I am talking here about some data taken
from the outside at start-up time, or maybe an IORef pointing to a mutable store that handles user sessions. The data stored here should help with the
business requirements of your API (storing user sessions' data, database pool, etc)

In Servant 0.5 was introduced the concept of `Context`. A `Context` is a way to pass values to the route combinators. The most popular usage of a
`Context` is to implement an authentication guard that is used next to route declarations in the API types. Basic Auth, JWT, OAuth are authentication
schemes that are best implemented through the use of a `Context`.

## Generics

Generics allow us describe APIs in records, and can be seen at work by rewriting the API definition we saw earlier: 

```haskell
data UserAPI mode = UserAPI
  { users  :: mode :- "version" :> Get '[JSON] [User]
  , albert :: mode :- "albert"  :> Get '[JSON] User
  , isaac  :: mode :- "isaac"   :> Get '[JSON] User
  } deriving stock Generic

server :: UserAPI (AsServerT APIMonad)
server = API { users = usersHandler
             , albert = albertHandler
             , isaac = isaacHandler
             }

usersHandler :: APIMonad [User]
usersHandler = pure [albert, isaac]

albertHandler :: APIMonad User
albertHandler = pure albert

isaacHandler :: APIMonad User
isaacHandler = pure isaac

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"
                (fromGregorian 1905 12 1)

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk"
                (fromGregorian 1683 3 1)
```

But API endpoints are scarcely this small. RESTful APIs aim to have trees endpoints for each resources of the system,
and as such we will not have everything on the top-level. In order to declare API trees with Generics, we proceed this way:

```Haskell
-- In API/Server/User.hs

module API.Server.User
  ( API
  , server
  ) where

import Servant
import Servant.API.Generic
import Servant.Server.Generic

import User.Types (User(..))

-- | 'mode' can be either a Server or a Client, which is a useful feature in Servant.
data API' mode = API
  { albert :: mode :- "albert" :> Get '[JSON] User
  , isaac  :: mode :- "isaac"  :> Get '[JSON] User
  } deriving stock Generic

-- | We use this type function to produce a type that can be used
-- as a sub-resource.
type API = ToServantApi API' 

server :: UserAPI (AsServerT APIMonad)
server = API { users = usersHandler
             , albert = albertHandler
             , isaac = isaacHandler
             }

-- Handlers

usersHandler :: APIMonad [User]
usersHandler = pure [albert, isaac]

albertHandler :: APIMonad User
albertHandler = pure albert

isaacHandler :: APIMonad User
isaacHandler = pure isaac

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"
                (fromGregorian 1905 12 1)

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk"
                (fromGregorian 1683 3 1)
```

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
app appState = 
  pure $ genericServeTWithContext
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

## Custom Monad

The above example shows that your handlers will have a type signature that uses your custom monad.
But what will our monad look like? We can start simple, and say that:

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

Let's use it to print the connection port of the server:

```haskell
loggedinHandler :: APIMonad Text
loggedinHandler = do
  port <- asks startupPort
  pure $ "Connection port is: " <> show port
```

[servant-url]: https://docs.servant.dev/en/stable/
[tutorial]: https://docs.servant.dev/en/stable/tutorial/Server.html
