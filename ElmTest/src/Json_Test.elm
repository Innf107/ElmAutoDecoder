module Json_Test exposing(..)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (..)

decodeTest : JD.Decoder Test
decodeTest = (JD.map3(\prop1 prop2 otherProp -> {prop1=prop1, prop2=prop2, otherProp=otherProp})
    (JD.field "prop1" JD.string)
    (JD.field "prop2" JD.string)
    (JD.field "otherProp" (JD.map2(\someProp someOtherProp -> {someProp=someProp, someOtherProp=someOtherProp})
        (JD.field "someProp" JD.int)
        (JD.field "someOtherProp" JD.string)
        ))
    )

decodeTest2 : JD.Decoder Test2
decodeTest2 = (JD.map4(\prop3 prop2 prop1 otherProp -> {prop3=prop3, prop2=prop2, prop1=prop1, otherProp=otherProp})
    (JD.field "prop3" JD.string)
    (JD.field "prop2" JD.string)
    (JD.field "prop1" JD.bool)
    (JD.field "otherProp" (JD.map2(\someProp someOtherProp -> {someProp=someProp, someOtherProp=someOtherProp})
        (JD.field "someProp" (JD.map2(\moreProps other -> {moreProps=moreProps, other=other})
            (JD.field "moreProps" (JD.list JD.int))
            (JD.field "other" (JD.list (JD.map(\p -> {p=p})
                (JD.field "p" JD.float)
                )))
            ))
        (JD.field "someOtherProp" JD.string)
        ))
    )

decodeTest3 : JD.Decoder Test3
decodeTest3 = (JD.map2(\prop1 prop2 -> {prop1=prop1, prop2=prop2})
    (JD.field "prop1" JD.string)
    (JD.field "prop2" decodeTest2)
    )

decodeT : JD.Decoder T
decodeT = (JD.field "tag" JD.string |> JD.andThen (\t -> case t of
        "A" -> JD.succeed A
        "B" -> JD.map B (JD.field "val0" JD.string)
        "C" -> JD.map3 C (JD.field "val0" JD.int)(JD.field "val1" JD.bool)(JD.field "val2" JD.string)
        _ -> JD.fail <| "unexpected tag " ++ t
    ))

