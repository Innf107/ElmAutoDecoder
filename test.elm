module Test exposing (..)

type alias Test = {
        prop1: String,
        prop2: String,
        otherProp: {
            someProp: Int,
            someOtherProp: String
        }
    }

type alias Test2 = {
        prop3: String,
        prop2: String,
        prop1: Bool,
        otherProp: {
            someProp: {
                moreProps: List Int,
                other: List {
                    p: Float
                }
            },
            someOtherProp: String
        }
    }