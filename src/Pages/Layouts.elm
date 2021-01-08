module Pages.Layouts exposing (..)


import Element exposing (centerX, centerY, column, layout, row, spacing)
centerLayout elements =
          layout [] <|
              row [ centerX, centerY ]
                  [   column [ centerX, spacing 10 ] elements
                  ]

twoColumnsLayout c1 c2 =
          layout [] <|
              row [ centerX, centerY, spacing 30 ]
                  [   column [ centerX, spacing 10 ] c1
                  ,   column [ centerX, spacing 10 ] c2
                  ]



centerXLayout elements =
          layout [] <|
              row [ centerX ]
                  [   column [ centerX, spacing 10 ] elements
                  ]
