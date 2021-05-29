-- | A gerritbot example config

let Config = ./src/Config.dhall

in  [ Config::{
      , roomId = "!NienFdxyssCkyajoyU:matrix.org"
      , projects = [ "zuul/*", "software-factory/*" ]
      }
    ]
