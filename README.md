Bwitterkuchen
----

CUI Twitter Client

- `app/Crendential.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Credential (twInfo) where

import Web.Twitter.Conduit

tokens :: OAuth
tokens = twitterOAuth
  { oauthConsumerKey = "***"
  , oauthConsumerSecret = "***"
  }

credential :: Credential
credential = Credential
  [ ("oauth_token", "***")
  , ("oauth_token_secret", "***")
  ]

twInfo :: TWInfo
twInfo = setCredential tokens credential def
```

