module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, p, text, textarea)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)



-- 1. MODEL


type alias Comment =
    { id : Int
    , author : String
    , body : String
    }


type alias Post =
    { id : Int
    , title : String
    , body : String
    , comments : List Comment
    , lastModified : Int
    }


-- WE REPLACED THE TUPLE WITH THIS CLEAN RECORD
type alias CommentInput =
    { targetPostId : Int
    , currentText : String
    }


type alias Model =
    { posts : List Post
    , nextPostId : Int
    , nextCommentId : Int
    , currentTime : Int
    -- Form States
    , newPostTitle : String
    , newPostBody : String
    , commentInputs : List CommentInput -- A clean list of records instead of tuples
    }


init : Model
init =
    { posts = []
    , nextPostId = 1
    , nextCommentId = 1
    , currentTime = 1
    , newPostTitle = ""
    , newPostBody = ""
    , commentInputs = []
    }



-- 2. UPDATE


type Msg
    = ChangePostTitle String
    | ChangePostBody String
    | SubmitPost
    | ChangeCommentBody Int String
    | SubmitComment Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePostTitle title ->
            { model | newPostTitle = title }

        ChangePostBody body ->
            { model | newPostBody = body }

        SubmitPost ->
            if String.isEmpty model.newPostTitle || String.isEmpty model.newPostBody then
                model

            else
                let
                    newPost =
                        { id = model.nextPostId
                        , title = model.newPostTitle
                        , body = model.newPostBody
                        , comments = []
                        , lastModified = model.currentTime
                        }
                in
                { model
                    | posts = newPost :: model.posts
                    , nextPostId = model.nextPostId + 1
                    , currentTime = model.currentTime + 1
                    , newPostTitle = ""
                    , newPostBody = ""
                }

        ChangeCommentBody postId typedText ->
            let
                -- Create a fresh input record for this specific post
                newInput =
                    { targetPostId = postId, currentText = typedText }

                -- Remove any old typing data for this post, then add the fresh one
                filteredInputs =
                    List.filter (\inputRecord -> inputRecord.targetPostId /= postId) model.commentInputs
            in
            { model | commentInputs = newInput :: filteredInputs }

        SubmitComment postId ->
            let
                -- Find the record matching this post ID to see what they typed
                matchingInput =
                    List.filter (\inputRecord -> inputRecord.targetPostId == postId) model.commentInputs
                        |> List.head

                -- Extract the text safely. If they typed nothing, default to empty string.
                commentText =
                    case matchingInput of
                        Just inputRecord ->
                            inputRecord.currentText

                        Nothing ->
                            ""
            in
            if String.isEmpty commentText then
                model

            else
                let
                    newComment =
                        { id = model.nextCommentId
                        , author = "Anonymous"
                        , body = commentText
                        }

                    -- Find the target post, append the comment, and update its timestamp
                    updatePost post =
                        if post.id == postId then
                            { post
                                | comments = post.comments ++ [ newComment ]
                                , lastModified = model.currentTime
                            }

                        else
                            post

                    updatedPosts =
                        List.map updatePost model.posts

                    -- Clear out the typing record for this post since it's now submitted
                    cleanedInputs =
                        List.filter (\inputRecord -> inputRecord.targetPostId /= postId) model.commentInputs
                in
                { model
                    | posts = updatedPosts
                    , nextCommentId = model.nextCommentId + 1
                    , currentTime = model.currentTime + 1
                    , commentInputs = cleanedInputs
                }



-- 3. VIEW


view : Model -> Html Msg
view model =
    let
        sortedPosts =
            List.sortBy .lastModified model.posts
                |> List.reverse
    in
    div [ style "max-width" "600px", style "margin" "40px auto", style "font-family" "sans-serif" ]
        [ h1 [] [ text "Simple Bulletin Board" ]
        
        -- New Post Form
        , div [ style "background" "#f4f4f4", style "padding" "20px", style "margin-bottom" "20px", style "border-radius" "5px" ]
            [ h2 [ style "margin-top" "0" ] [ text "Create a Post" ]
            , input [ placeholder "Title", value model.newPostTitle, onInput ChangePostTitle, style "width" "100%", style "margin-bottom" "10px", style "padding" "8px" ] []
            , textarea [ placeholder "What's on your mind?", value model.newPostBody, onInput ChangePostBody, style "width" "100%", style "height" "80px", style "margin-bottom" "10px", style "padding" "8px" ] []
            , button [ onClick SubmitPost, style "padding" "8px 16px", style "background" "#0074D9", style "color" "white", style "border" "none", style "cursor" "pointer" ] [ text "Post" ]
            ]
        
        -- Posts List
        , div [] (List.map (viewPost model.commentInputs) sortedPosts)
        ]


viewPost : List CommentInput -> Post -> Html Msg
viewPost allInputs post =
    let
        -- Look up what is currently typed for this post's comment box
        matchingInput =
            List.filter (\inputRecord -> inputRecord.targetPostId == post.id) allInputs
                |> List.head

        boxText =
            case matchingInput of
                Just inputRecord ->
                    inputRecord.currentText

                Nothing ->
                    ""
    in
    div [ style "border" "1px solid #ddd", style "padding" "20px", style "margin-bottom" "20px", style "border-radius" "5px" ]
        [ h3 [ style "margin" "0 0 10px 0" ] [ text post.title ]
        , p [] [ text post.body ]
        
        -- Comments Section
        , div [ style "margin-left" "20px", style "border-left" "2px solid #eee", style "padding-left" "15px" ]
            [ h3 [ style "font-size" "1em", style "color" "#777" ] [ text "Comments" ]
            , div [] (List.map viewComment post.comments)
            
            -- Add Comment Form
            , div [ style "margin-top" "10px" ]
                [ input
                    [ placeholder "Write a comment..."
                    , value boxText
                    , onInput (ChangeCommentBody post.id)
                    , style "width" "70%", style "padding" "5px", style "margin-right" "10px"
                    ]
                    []
                , button [ onClick (SubmitComment post.id), style "padding" "5px 10px" ] [ text "Reply" ]
                ]
            ]
        ]


viewComment : Comment -> Html Msg
viewComment comment =
    div [ style "background" "#fafafa", style "padding" "8px", style "margin-bottom" "5px", style "border-radius" "3px" ]
        [ p [ style "margin" "0", style "font-size" "0.9em" ] [ text comment.body ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
