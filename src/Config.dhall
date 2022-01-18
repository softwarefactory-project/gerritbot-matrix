-- | The gerritbot channel configuration
let Event = < PatchsetCreated | ChangeMerged | ChangeReady >

in  { Type =
        { room : Text
        , projects : List Text
        , branches : List Text
        , servers : List Text
        , events : List Event
        }
    , default =
      { branches = [ "main", "master" ]
      , servers = [ "*" ]
      , events =
        [ Event.PatchsetCreated, Event.ChangeMerged, Event.ChangeReady ]
      }
    , Event
    }
