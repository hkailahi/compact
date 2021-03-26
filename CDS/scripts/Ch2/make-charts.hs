import Control.Lens (set)
import Control.Lens.Prism (_Just)
import Data.Colour.Names (blueviolet, crimson)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Graphics.Rendering.Chart.Easy
  ( Default (def),
    FillStyle (FillStyleSolid),
    addIndexes,
    autoIndexAxis,
    axis_label_style,
    bars,
    font_color,
    font_size,
    laxis_generate,
    layout_axes_styles,
    layout_legend,
    layout_title,
    layout_title_style,
    layout_x_axis,
    legend_label_style,
    opaque,
    plot,
    plotBars,
    plot_bars_item_styles,
    (.=),
  )
import Relude

-- Modeled off example: https://github.com/timbod7/haskell-chart/wiki/example-11

titles :: [String]
titles = ["Occurences"]

-- "Huffman coding" is a common zero-order compressor. For example, a possible huffman coding of
-- @"abracadabra"@ could be:
-- <<images/LangmeadEmpiricalHuffman.png>>
values :: [(String, [Double])]
values =
  [ ("a", [5]),
    ("b", [2]),
    ("c", [1]),
    ("d", [1]),
    ("r", [2])
  ]

main :: IO ()
main = toFile def "images/generated/huffman-bar-chart.png" $ do
  layout_title .= "Huffman Coding: " <> "\"abracadabra\""
  layout_title_style . font_size .= 40
  layout_legend . _Just . legend_label_style . font_size .= 18
  layout_axes_styles . axis_label_style . font_size .= 18
  layout_axes_styles . axis_label_style . font_color .= opaque crimson
  layout_x_axis . laxis_generate .= autoIndexAxis (fmap fst values)
  let barStyle = [(FillStyleSolid (opaque blueviolet), Nothing)]
  plot
    . fmap (plotBars . set plot_bars_item_styles barStyle)
    . bars titles
    . addIndexes
    $ fmap snd values
