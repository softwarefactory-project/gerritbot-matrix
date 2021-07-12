-- | A gerritbot example config

let Config = ./src/Config.dhall

let rdo-configs = [ "config", "rdo-jobs", "rdoinfo", "rdo_gating_scripts" ]

in  [ Config::{
      , room = "#gerritbot:matrix.org"
      , projects =
        [ "zuul/*"
        , "software-factory/*matrix*"
        , "software-factory/*gerrit*"
        , "opendev/*"
        ]
      }
    , Config::{
      , room = "#softwarefactory-project:matrix.org"
      , projects = [ "software-factory/*", "rpms/*" ]
      , servers = [ "softwarefactory-project.io" ]
      }
    , Config::{
      , room = "!RVsCBSBPpQxhCyVEcB:matrix.org"
      , projects = [ "rdo-infra/*" ] # rdo-configs
      , servers = [ "review.rdoproject.org" ]
      }
    , Config::{
      , room = "!SoKrrlUlCZgGfhjYsr:matrix.org"
      , projects = [ "openstack/*", "puppet/*" ] # rdo-configs
      , servers = [ "review.rdoproject.org" ]
      }
    ]
