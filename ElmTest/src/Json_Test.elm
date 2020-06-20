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



encodeTest : Test -> JE.Value
encodeTest = (\x1 -> JE.object [("prop1", JE.string x1.prop1), ("prop2", JE.string x1.prop2), ("otherProp", (\x2 -> JE.object [("someProp", JE.int x2.someProp), ("someOtherProp", JE.string x2.someOtherProp)]) x1.otherProp)])

encodeTest2 : Test2 -> JE.Value
encodeTest2 = (\x1 -> JE.object [("prop3", JE.string x1.prop3), ("prop2", JE.string x1.prop2), ("prop1", JE.bool x1.prop1), ("otherProp", (\x2 -> JE.object [("someProp", (\x3 -> JE.object [("moreProps", JE.list (JE.int)  x3.moreProps), ("other", JE.list ((\x5 -> JE.object [("p", JE.float x5.p)]))  x3.other)]) x2.someProp), ("someOtherProp", JE.string x2.someOtherProp)]) x1.otherProp)])

encodeTest3 : Test3 -> JE.Value
encodeTest3 = (\x1 -> JE.object [("prop1", JE.string x1.prop1), ("prop2", encodeTest2 x1.prop2)])

encodeT : T -> JE.Value
encodeT = \x1 -> case x1 of 
    A -> JE.object [("tag", JE.string "A")]
    B val0 -> JE.object [("tag", JE.string "B"), ("val0", JE.string val0)]
    C val0 val1 val2 -> JE.object [("tag", JE.string "C"), ("val0", JE.int val0), ("val1", JE.bool val1), ("val2", JE.string val2)]

