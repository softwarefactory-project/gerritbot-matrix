-- | The gerritbot channel configuration
{ Type = { roomId : Text, projects : List Text, branches : List Text }
, default.branches = [ "main", "master" ]
}
