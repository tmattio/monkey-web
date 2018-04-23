module Route exposing (Route(..), href, modifyUrl, fromLocation)

import UrlParser as Url exposing (parseHash, s, (</>), string, oneOf, Parser)
import Navigation exposing (Location)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import Regex exposing (..)


-- ROUTING --


type Route
    = Home
    | Features
    | OurVision
    | Search
    | Login
    | Logout
    | Register
    | Explore
    | Pricing
    | Profile String
    | PreviewDataset String String
    | InsightsDataset String String
    | SettingsDataset String String
    | Label String String
    | CreateDataset
    | Settings



--    When needing parameters on the form base/item/id
--   | Item String


routeMatcher : Parser (Route -> a) a
routeMatcher =
    oneOf
        [ Url.map Home (s "")
        , Url.map Features (s "features")
        , Url.map OurVision (s "our-vision")
        , Url.map Search (s "search")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Register (s "register")
        , Url.map Settings (s "settings")
        , Url.map Explore (s "explore")
        , Url.map Pricing (s "pricing")
        , Url.map CreateDataset (s "new")
        , Url.map Profile (string)
        , Url.map PreviewDataset (string </> string)
        , Url.map InsightsDataset (string </> string </> s "insights")
        , Url.map SettingsDataset (string </> string </> s "settings")
        , Url.map Label (string </> string </> s "label")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pagePath =
            case page of
                Home ->
                    []

                Features ->
                    [ "features" ]

                OurVision ->
                    [ "our-vision" ]

                Search ->
                    [ "search" ]

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]

                Explore ->
                    [ "explore" ]

                Pricing ->
                    [ "pricing" ]

                Profile username ->
                    [ username ]

                CreateDataset ->
                    [ "new" ]

                PreviewDataset username datasetName ->
                    [ username, slugify datasetName ]

                InsightsDataset username datasetName ->
                    [ username, slugify datasetName, "insights" ]

                SettingsDataset username datasetName ->
                    [ username, slugify datasetName, "settings" ]

                Label username datasetName ->
                    [ username, slugify datasetName, "label" ]
    in
        "#/" ++ (String.join "/" pagePath)



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash routeMatcher location


slugify : String -> String
slugify =
    String.toLower
        >> replace All (regex "\\s+") (\_ -> "-")
        >> replace All (regex "[^\\w\\-]+") (\_ -> "")
        >> replace All (regex "\\-\\-+") (\_ -> "-")
        >> replace All (regex "^-+") (\_ -> "")
        >> replace All (regex "-+$") (\_ -> "")
