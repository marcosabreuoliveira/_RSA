os2ip teste = sum [(fromEnum (fst x)) * 256 ^ (snd x) | x <- (zip teste [0..(length teste -1)])]