{-# LANGUAGE RecordWildCards #-}

import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple

data Config = Config { theta :: Vector Double
                     , alpha :: Double
                     , iterations :: Int }

defaultConfig = Config (2 |> repeat 0) 0.01 1500

dispv v = putStrLn $ vecdisp (disps 3) v

dispm m = putStrLn $ disps 3 m

sumColumns = fromList . map sumElements . toColumns

replicateColumns n = fromColumns . replicate n

addOnesColumn x = fromColumns [dim x |> repeat 1, x]

computeCost Config {..} x y = s / (2 * m)
  where s = sumElements $ (x' <> theta - y) ^ 2
        m = fromIntegral $ dim y
        x' = addOnesColumn x

gradientDescent Config {..} x y = gradient 0 theta
    where gradient i t = if i >= iterations
                         then t
                         else gradient i' t'
               where i' = i + 1
                     t' = t - (scalar alpha * s / m)
                     s = sumColumns $ replicateColumns (cols x') x'' * x'
                     x'' = x' <> t - y
          m = fromIntegral $ dim y
          x' = addOnesColumn x

mkPath typ x y = (style, points)
  where style = defaultStyle {plotType = typ}
        points = zip (toList x) (toList y)

plot = plotPathsStyle []

predict p t = (* 10000) . head . toList $ fromColumns [1, p] <> t

main :: IO ()
main = do
    [x, y] <- (toColumns . readMatrix) `fmap` readFile "data1.txt"

    putStr "Initial cost: "
    print $ computeCost defaultConfig x y

    let t = gradientDescent defaultConfig x y
    putStr "Theta found by gradient descent: "
    dispv t

    plot [mkPath Points x y
         ,mkPath Lines x (addOnesColumn x <> t)]

    putStr "For population = 35,000 we predict a profit of "
    print $ predict 3.5 t

    putStr "For population = 70,000 we predict a profit of "
    print $ predict 7 t
