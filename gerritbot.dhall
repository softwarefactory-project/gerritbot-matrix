-- | A gerritbot example config

let Config = ./src/Config.dhall

let rdo-configs = [ "config", "rdo-jobs", "rdoinfo", "rdo_gating_scripts" ]

in  [ Config::{
      , roomId = "!NienFdxyssCkyajoyU:matrix.org"
      , projects =
        [ "zuul/*"
        , "software-factory/*matrix*"
        , "software-factory/*gerrit*"
        , "opendev/*"
        ]
      }
    , Config::{
      , roomId = "!vjToxuubYwiQCrhgDW:matrix.org"
      , projects = [ "software-factory/*", "rpms/*" ]
      , servers = [ "softwarefactory-project.io" ]
      }
    , Config::{
      , roomId = "!RVsCBSBPpQxhCyVEcB:matrix.org"
      , projects = [ "rdo-infra/*" ] # rdo-configs
      , servers = [ "review.rdoproject.org" ]
      }
    , Config::{
      , roomId = "!SoKrrlUlCZgGfhjYsr:matrix.org"
      , projects = [ "openstack/*", "puppet/*" ] # rdo-configs
      , servers = [ "review.rdoproject.org" ]
      }
    ]
