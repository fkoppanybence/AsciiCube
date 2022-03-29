--Nev: Fulop Koppany-Bence

import Data.Char
import Control.Monad

convert_ h w d = (1 + h + d, w * 2 + h + d)

spacek size
    | size == 0 = []
    | otherwise = ' ' : spacek (size - 1)

rajzol_felso_ (x:xs) d hany
    | d == 0 = (take hany (repeat '_')) ++ (drop hany (x:xs))
    | otherwise = x : (rajzol_felso_ xs (d-1) hany)

rajzol_felsoo (x:xs) d hany
    | d == 0 = (take hany (repeat '.')) ++ (drop hany (x:xs))
    | otherwise = x : (rajzol_felsoo xs (d-1) hany)

rajzol_felsox xs ind hany m num
    | (hany == 0) = xs
    | otherwise = (take (ind-1) xs) ++ [chr num] ++ (rajzol_felsox (drop ind xs) (m + 1) (hany-1) m num)

rajzol_felsoxx xs ind hany m
    | (hany == 0) = xs
    | otherwise = (take (ind-1) xs) ++ ['⠡'] ++ (rajzol_felsoxx (drop ind xs) (m + 1) (hany-1) m)

rajzol_felsoy xs ind hany m num
    | (hany == 0) = xs
    | otherwise = (take (ind-1) xs) ++ [chr num] ++ (rajzol_felsoy (drop ind xs) (m - 1) (hany-1) m num)

rajzol_felsoyy xs ind hany m
    | (hany == 0) = xs
    | otherwise = (take (ind-1) xs) ++ ['⠌'] ++ (rajzol_felsoyy (drop ind xs) (m - 1) (hany-1) m)


torol (x:xs)
    | x == ' ' = torol xs
    |otherwise = (reverse (x:xs))

main :: IO ()
main = do
    input_line <- getLine
    let w = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    input_line <- getLine
    let d = read input_line :: Int
    let n = 1 + h + d
    let m = w*2 + h + d
    let szam = n*m

    if (w==1 && d==1)
        then do let ossz1 =  rajzol_felsoyy (spacek szam) ((h+1)*m+d+h) d m
                let ossz2 = rajzol_felsoxx ossz1 ((2*m)+d+2) (h-1) m
                let ossz3 = rajzol_felso_  ossz2 d (2*w)
                let ossz4 = rajzol_felsoo ossz3 (h*m+d+h) (2*w-2)
                let ossz5 = rajzol_felsox ossz4 (2*m-h+1) h m 92
                let ossz6 = rajzol_felso_ ossz5 (m*d) (2*w)
                let ossz7 = rajzol_felsox ossz6 (m*(d+1)+1) h m 92
                let ossz8 = rajzol_felsoy ossz7 (m+d) d m 47
                let ossz9 = rajzol_felsoy ossz8 ((2+h)*m) d m 47
                let ossz10 = rajzol_felso_ ossz9 (m*(n-1) + h) (2*w)
                let ossz11 = rajzol_felsox ossz10 (m*(d+1)+ 1 + 2*w) h m 92
                let ossz = rajzol_felsoy ossz11 (2*m-h) d m 47
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = torol ( reverse( take m (drop (i*m) ossz) )  )
                    return n
                mapM putStrLn sor
    else if (w==1)
        then do let ossz1 =  rajzol_felsoyy (spacek szam) ((h+1)*m+d+h) d m
                let ossz2 = rajzol_felsoxx ossz1 ((2*m)+d+2) (h-1) m
                let ossz3 = rajzol_felso_  ossz2 d (2*w)
                let ossz4 = rajzol_felsoo ossz3 (h*m+d+h) (2*w)
                let ossz5 = rajzol_felsox ossz4 (2*m-h+1) h m 92
                let ossz6 = rajzol_felso_ ossz5 (m*d) (2*w)
                let ossz7 = rajzol_felsox ossz6 (m*(d+1)+1) h m 92
                let ossz8 = rajzol_felsoy ossz7 (m+d) d m 47
                let ossz9 = rajzol_felsoy ossz8 ((2+h)*m) d m 47
                let ossz10 = rajzol_felso_ ossz9 (m*(n-1) + h) (2*w)
                let ossz11 = rajzol_felsox ossz10 (m*(d+1)+ 1 + 2*w) h m 92
                let ossz = rajzol_felsoy ossz11 (2*m-h) d m 47
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = torol ( reverse( take m (drop (i*m) ossz) )  )
                    return n
                mapM putStrLn sor
    else if (h==1)
        then do let ossz2 = rajzol_felsoxx (spacek szam) (m+d+1) h m
                let ossz3 = rajzol_felso_  ossz2 d (2*w)
                let ossz4 = rajzol_felsoo ossz3 (h*m+d+h) (2*w)
                let ossz5 = rajzol_felsox ossz4 (2*m-h+1) h m 92
                let ossz6 = rajzol_felso_ ossz5 (m*d) (2*w)
                let ossz7 = rajzol_felsox ossz6 (m*(d+1)+1) h m 92
                let ossz8 = rajzol_felsoy ossz7 (m+d) d m 47
                let ossz9 = rajzol_felsoy ossz8 ((2+h)*m) d m 47
                let ossz10 = rajzol_felso_ ossz9 (m*(n-1) + h) (2*w)
                let ossz11 = rajzol_felsox ossz10 (m*(d+1)+ 1 + 2*w) h m 92
                let ossz12 =  rajzol_felsoyy ossz11 ((h+1)*m+d+h) (d-1) m
                let ossz = rajzol_felsoy ossz12 (2*m-h) d m 47
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = torol ( reverse( take m (drop (i*m) ossz) )  )
                    return n
                mapM putStrLn sor
    else if (d==1)
        then do let ossz2 = rajzol_felsoxx (spacek szam) (m+d+1) h m
                let ossz3 = rajzol_felso_  ossz2 d (2*w)
                let ossz4 = rajzol_felsoo ossz3 (h*m+d+h) (2*w-2)
                let ossz5 = rajzol_felsox ossz4 (2*m-h+1) h m 92
                let ossz6 = rajzol_felso_ ossz5 (m*d) (2*w)
                let ossz7 = rajzol_felsox ossz6 (m*(d+1)+1) h m 92
                let ossz8 = rajzol_felsoy ossz7 (m+d) d m 47
                let ossz9 = rajzol_felsoy ossz8 ((2+h)*m) d m 47
                let ossz10 = rajzol_felso_ ossz9 (m*(n-1) + h) (2*w)
                let ossz11 = rajzol_felsox ossz10 (m*(d+1)+ 1 + 2*w) h m 92
                let ossz12 =  rajzol_felsoyy ossz11 ((h+1)*m+d+h) d m
                let ossz = rajzol_felsoy ossz12 (2*m-h) d m 47
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = torol ( reverse( take m (drop (i*m) ossz) )  )
                    return n
                mapM putStrLn sor      
        else do let ossz1 = rajzol_felso_ (spacek szam) d (2*w)
                let ossz2 = rajzol_felsoo ossz1 (h*m+d+h) (2*w)
                let ossz3 = rajzol_felsox ossz2 (2*m-h+1) h m 92
                let ossz4 = rajzol_felso_ ossz3 (m*d) (2*w)
                let ossz5 = rajzol_felsox ossz4 (m*(d+1)+1) h m 92
                let ossz6 = rajzol_felsoy ossz5 (m+d) d m 47
                let ossz7 = rajzol_felsoy ossz6 ((2+h)*m) d m 47
                let ossz8 = rajzol_felso_ ossz7 (m*(n-1) + h) (2*w)
                let ossz9 =  rajzol_felsoyy ossz8 ((h+1)*m+d+h) d m
                let ossz10 = rajzol_felsoxx ossz9 (m+d+1) h m
                let ossz11 = rajzol_felsox ossz10 (m*(d+1)+ 1 + 2*w) h m 92
                let ossz = rajzol_felsoy ossz11 (2*m-h) d m 47
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = torol ( reverse( take m (drop (i*m) ossz) )  )
                    return n
                mapM putStrLn sor

    return ()