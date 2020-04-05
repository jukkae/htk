module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Note

width, height :: Int
width = 800
height = width

data Vertex = Vertex
  { x :: Float
  , y :: Float
  } deriving (Eq)

window :: Display
-- window = InWindow "HTK" (width, height) (10, 10)
window = FullScreen

circsFromTonality :: Tonality -> [Picture]
circsFromTonality tonality =
  map (\v -> translate (x v) (y v) $ color white $ circleSolid 10.0) vertices where vertices = map vertexFromNote (notes tonality)

tonicCircleFromTonality :: Tonality -> [Picture]
tonicCircleFromTonality tonality =
  [translate (x v) (y v) $ color white $ thickCircle 30.0 5.0] where v = vertexFromNote (tonic tonality)

drawThickLine :: Vertex -> Vertex -> Color -> Float -> Picture
drawThickLine a b c w = 
  color c $ Polygon [a0, a1, b1, b0] where
    a0 = (x a + fst v, y a + snd v)
    b0 = (x b + fst v, y b + snd v)
    a1 = (x a - fst v, y a - snd v)
    b1 = (x b - fst v, y b - snd v)
    
    v = (fst nNorm * width * 0.5, snd nNorm * width * 0.5)
    ab = (x b - x a, y b - y a)
    n = (y b - y a, (-1.0) * (x b - x a))
    nNorm = (fst n / magnitude, snd n / magnitude)
    magnitude = sqrt((fst n * fst n) + (snd n * snd n))
    width = w

drawLine :: Vertex -> Vertex -> Picture
drawLine a b =
  drawThickLine a b c w where
    c = makeColor 1.0 1.0 1.0 0.7
    w = 1.5

linesFromTonality :: Tonality -> [Picture]
linesFromTonality tonality = [drawIntervalLine i | i <- getIntervals tonality] where
  drawIntervalLine i = drawLine x y where
    x = vertexFromNote (a i)
    y = vertexFromNote (b i)

axes :: [Picture]
axes = [drawThickLine (Vertex 0 0) (Vertex 0 500) c 2.0, drawThickLine (Vertex 0 0) (Vertex 500 0) c 2.0] where c = makeColor 1.0 1.0 1.0 0.7

diagramFromTonality :: Tonality -> [Picture]
diagramFromTonality t = concat [axes, linesFromTonality t, circsFromTonality t, tonicCircleFromTonality t]

textsFromTonality :: Tonality -> [Picture]
textsFromTonality tonality =
  [ writeAt 400 300 "note set:"
  , writeAt 400 260 noteSetString
  , writeAt 400 200 "tonic:"
  , writeAt 400 160 tonicString
  , writeAt (-900) 540 "tris:"
  , writeAt (-900) 500 trichordsString
  , writeAt (-900) 340 "harmonious tris:"
  , writeAt (-900) 300 harmoniousTrichordsString
  , writeAt (-900) 200 "raw intervals:"
  , writeAt (-900) 160 intervalSetString
  , writeAt (-900) 100 "interval vector:"
  , writeAt (-900) 60 intervalVectorString
  ]
  where
    writeAt x y s = translate x y $ write s
    write s = scale 0.2 0.2 $ color white $ Text s

    trichords = getTrichords tonality
    harmoniousTris = packAndOrderTris (filter isHarmonious trichords) -- This is broken!
    intervals = getIntervals tonality

    trichordsString = concat [printTrichord t | t <- trichords ]
    noteSetString = printTonalityNotes tonality
    tonicString = printNote (tonic tonality)

    harmoniousTrichordsString = concat [printTrichord t | t <- harmoniousTris]
    
    intervalSetString = concat [printInterval t | t <- intervals]

    intervalVectorString = show (getIntervalVector tonality)

displayNotes :: Tonality -> Picture
displayNotes ns =
  pictures (concat [diagramFromTonality ns, textsFromTonality ns])

vertexFromNote :: Note -> Vertex
vertexFromNote n = Vertex x y where
  x = sin(2.0 / 12.0 * pi * fromIntegral n) * r
  y = cos(2.0 / 12.0 * pi * fromIntegral n) * r
  r = 400.0

handleInputs :: Event -> Tonality -> Tonality
-- toggle individual pitch class
handleInputs (EventKey (Char 'z') Down _ _) w = toggleSelected w 0
handleInputs (EventKey (Char 's') Down _ _) w = toggleSelected w 1
handleInputs (EventKey (Char 'x') Down _ _) w = toggleSelected w 2
handleInputs (EventKey (Char 'd') Down _ _) w = toggleSelected w 3
handleInputs (EventKey (Char 'c') Down _ _) w = toggleSelected w 4
handleInputs (EventKey (Char 'v') Down _ _) w = toggleSelected w 5
handleInputs (EventKey (Char 'g') Down _ _) w = toggleSelected w 6
handleInputs (EventKey (Char 'b') Down _ _) w = toggleSelected w 7
handleInputs (EventKey (Char 'h') Down _ _) w = toggleSelected w 8
handleInputs (EventKey (Char 'n') Down _ _) w = toggleSelected w 9
handleInputs (EventKey (Char 'j') Down _ _) w = toggleSelected w 10
handleInputs (EventKey (Char 'm') Down _ _) w = toggleSelected w 11

-- other actions
handleInputs (EventKey (Char 'q') Down _ _) w = flipSelection w
handleInputs (EventKey (Char 'w') Down _ _) w = transposeTonality w (-1)
handleInputs (EventKey (Char 'e') Down _ _) w = transposeTonality w 1
handleInputs (EventKey (Char 'r') Down _ _) w = resetTonality w

-- otherwise do nothing
handleInputs _ w = w

updateNotes :: Float -> Tonality -> Tonality
updateNotes dt w = w

main :: IO ()
main =
  play
  window bgColor fps -- display mode, bg color, FPS
  initialTonality -- initial world
  displayNotes -- rendering action
  handleInputs -- input handler
  updateNotes -- step world one iteration
  where
    bgColor = black
    fps = 60
