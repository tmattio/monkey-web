module Page.Static.OurVision exposing (view)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (alt, class, id, src, tabindex, type_, attribute)
import Data.Auth exposing (Session)


view : Maybe Session -> Html msg
view session =
    main_ [ id "content", class "container page", tabindex -1 ]
        [ div [ class "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" ]
            [ h1 [ class "display-4" ]
                [ text "Our Vision" ]
            ]
        , div [ class "our-vision-content" ]
            [ p [] [ text "The potential of AI is immense. It is in the process of revolutionizing all industries and it is supporting today's world-changing products. Probably the most amazing thing about AI  is that it only needs two things to have the effect it has: models and data. Research on the former  is actively done by academia and world-leading companies such as Facebook or Google. As for the latter, access to data is getting easier and easier, but we can do better!" ]
            , p [] [ text "We can make a platform that removes all friction from building and working with datasets. This platform will empower all researchers, developers and managers who work on AI, allowing them to focus on creating value rather than reinventing the wheel every time they want to build something new." ]
            , p [] [ text "Imagine what would happen if:" ]
            , ul []
                [ li [] [ text "An AI researcher who wants to validate his new model could find any task-specific data and download it in a format of his choice in under two minutes." ]
                , li [] [ text "A developer working on an AI project could easily label any kind of data and receive useful insights on how he could improve his datasets." ]
                , li [] [ text "A project manager who needs to create a dataset with millions of labels could send a link to hired labelers, have access to statistics on their progress, and even automate their payroll based on these statistics." ]
                ]
            , p [] [ text "This is exactly what we are building at Monkey: a platform that you can trust to be your best ally when working with AI." ]
            , p [] [ text "And because we also believe that people work better together, we've made collaboration the core foundation of Monkey. Here, you will be able to use open projects, explore popular ones and collaborate with people all around the world to create the greatest bank of datasets." ]
            , p [] [ text "Together, let's built the future of AI!" ]
            ]
        ]
