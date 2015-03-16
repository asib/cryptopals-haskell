import Crypto
import System.IO
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  withFile "6.txt" ReadMode (\handle -> do
    c <- hGetContents handle
    contents <- return . C8.unpack . fromBase64 . concat . lines $ c
    mapM_ print $ decodeRepeatingKeyXor contents
    )

    -- "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \n
    -- In ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin'
    --  hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm l
    -- ettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let m
    -- y concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that'
    -- s the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo
    -- get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase
    -- \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n
    -- ' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the be
    -- at play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on so
    -- me wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocio
    -- us \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\n
    -- I'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut y
    -- ou'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if
    --  you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my
    -- God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP.
    --  Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like
    --  a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you p
    -- eople are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groo
    -- vin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you'r
    -- e amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the b
    -- ases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic
    -- , I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou migh
    -- t have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \
    -- n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I ca
    -- n smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You c
    -- an call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor
    -- , Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle
    -- me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo c
    -- ome on, everybody and sing this song \n\nSay -- Play that funky music Say, go white bo
    -- y, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down a
    -- nd boogie and play that funky music till you die. \n\nPlay that funky music Come on, C
    -- ome on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that f
    -- unky music A little louder now \nPlay that funky music, white boy Come on, Come on, Co
    -- me on \nPlay that funky music \n"
