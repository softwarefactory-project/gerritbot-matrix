-- | The gerritbot channel configuration
let Event = ./EventType.dhall

in  { Type =
        { roomId : Text
        , projects : List Text
        , branches : List Text
        , events : List Event
        }
    , default =
      { branches = [ "main", "master" ]
      , events = [ Event.PatchsetCreated, Event.ChangeMerged ]
      }
    }
