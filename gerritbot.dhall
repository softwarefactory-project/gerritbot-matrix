-- | A gerritbot example config

let Config = ./src/Config.dhall

in  [ Config::{
      , roomId = "!NienFdxyssCkyajoyU:matrix.org"
      , projects = [ "zuul/*", "software-factory/*" ]
      }
    , Config::{
      , roomId = "!RVsCBSBPpQxhCyVEcB:matrix.org"
      , projects = [ "*" ]
      , servers = [ "review.rdoproject.org" ]
      }
    ]
