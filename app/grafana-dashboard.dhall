{-| Grafana dashboard configuration for gerritbot-matrix

Set the prometheus datasource name using the `GRAFANA_DATASOURCE` environment variable name.

Apply using [grafdhall](https://github.com/softwarefactory-project/grafdhall) or import the json:

  dhall-to-json < ./grafana-dashboard.dhall
-}
let Grafana =
      https://raw.githubusercontent.com/weeezes/dhall-grafana/f78d2887939dcb555a47a4b85a91a3d6b38ec2ea/package.dhall sha256:a0e1b5432090944fa671efce0085c6049019ae0d00ca289c268b4528d1cd39af

let datasource = Some (env:GRAFANA_DATASOURCE as Text ? "Prometheus")

let counter =
      \(refId : Text) ->
      \(name : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr = "increase(${name}[5m])"
          , legendFormat = Some "${title} {{ job }}"
          }

let gauge =
      \(refId : Text) ->
      \(expr : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr
          , legendFormat = Some "${title} {{ job }}"
          }

let panel =
      \(y : Natural) ->
      \(title : Text) ->
      \(targets : List Grafana.MetricsTargets) ->
        Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title
          , gridPos = { x = 0, y, w = 24, h = 6 }
          , datasource
          , targets
          , fill = 0
          , linewidth = 2
          }

let panels =
      [ panel
          0
          "Activity"
          [ counter "A" "gerrit_events" "Gerrit events received"
          , counter "B" "matrix_messages" "Matrix messages sent"
          ]
      , panel
          8
          "Health"
          [ counter "A" "gerrit_errors" "Gerrit connection errors"
          , counter "B" "matrix_errors" "Matrix post failures"
          ]
      , panel
          16
          "Memory"
          [ gauge
              "A"
              "ghc_gcdetails_live_bytes"
              "Total amount of live data in the heap."
          ]
      , panel 24 "CPU" [ counter "B" "ghc_cpu_seconds_total" "CPU time" ]
      ]

in  Grafana.Dashboard::{
    , title = "Gerritbot Matrix"
    , editable = True
    , panels = Grafana.Utils.generateIds panels
    , links =
      [ Grafana.Link.Type.Link
          Grafana.LinkExternal::{
          , title = "gerritbot-matrix"
          , url = "https://github.com/softwarefactory-project/gerritbot-matrix"
          , tooltip = "gerritbot for matrix"
          }
      ]
    }
