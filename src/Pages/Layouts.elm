module Pages.Layouts exposing (..)


import Element exposing (centerX, centerY, column, layout, row, spacing)
centerLayout elements =
          layout [] <|
              row [ centerX, centerY ]
                  [   column [ centerX, spacing 10 ] elements
                  ]

centerXLayout elements =
          layout [] <|
              row [ centerX ]
                  [   column [ centerX, spacing 10 ] elements
                  ]
