-- | A gerritbot example config

let Config = ./src/Config.dhall

let rdo-configs = [ "config", "rdo-jobs", "rdoinfo", "rdo_gating_scripts" ]

in  [ Config::{
      , room = "#gerritbot:matrix.org"
      , projects = [ "software-factory/*matrix*", "software-factory/*gerrit*" ]
      }
    , Config::{
      , room = "#softwarefactory-project:matrix.org"
      , projects = [ "software-factory/*", "rpms/*" ]
      , servers = [ "softwarefactory-project.io" ]
      }
    ]
