module DecoderTypes where

data Decoder = Native String
             | Custom String
             | Record [(String, Decoder)]
             | List Decoder
             | ADTD [(String, [Decoder])]

             