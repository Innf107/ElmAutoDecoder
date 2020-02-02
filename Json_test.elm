module Json_test exposing(..)
import Json.Decode as JD
import Json.Encode as JE

testDecoder : JD.Decoder Test
testDecoder = (JD.map3 (\prop1 prop2 otherProp -> {prop1=prop1, prop2=prop2, otherProp=otherProp})
    (JD.field "prop1" JD.string)
    (JD.field "prop2" JD.string)
    (JD.field "otherProp" (JD.map2 (\someProp someOtherProp -> {someProp=someProp, someOtherProp=someOtherProp})
        (JD.field "someProp" JD.int)
        (JD.field "someOtherProp" JD.string))))

testEncoder : Test -> JE.Value
testEncoder = \x0 -> (JE.object [(prop1, JE.string x0.prop1), (prop2, JE.string x0.prop2), (otherProp, \x1 -> (JE.object [(someProp, JE.int x1.someProp), (someOtherProp, JE.string x1.someOtherProp)]) x0.otherProp)])


test2Decoder : JD.Decoder Test2
test2Decoder = (JD.map4 (\prop3 prop2 prop1 otherProp -> {prop3=prop3, prop2=prop2, prop1=prop1, otherProp=otherProp})
    (JD.field "prop3" JD.string)
    (JD.field "prop2" JD.string)
    (JD.field "prop1" JD.bool)
    (JD.field "otherProp" (JD.map2 (\someProp someOtherProp -> {someProp=someProp, someOtherProp=someOtherProp})
        (JD.field "someProp" (JD.map2 (\moreProps other -> {moreProps=moreProps, other=other})
            (JD.field "moreProps" (JD.list JD.int))
            (JD.field "other" (JD.list (JD.map (\p -> {p=p})
                (JD.field "p" JD.float))))))
        (JD.field "someOtherProp" JD.string))))

test2Encoder : Test2 -> JE.Value
test2Encoder = \x0 -> (JE.object [(prop3, JE.string x0.prop3), (prop2, JE.string x0.prop2), (prop1, JE.bool x0.prop1), (otherProp, \x1 -> (JE.object [(someProp, \x2 -> (JE.object [(moreProps, (JE.list JE.int) x2.moreProps), (other, (JE.list \x3 -> (JE.object [(p, JE.float x3.p)])) x2.other)]) x1.someProp), (someOtherProp, JE.string x1.someOtherProp)]) x0.otherProp)])