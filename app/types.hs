module Parley.Types (
                    , Topic (getTopic)
                    , CommentText (getComent)
                    , mkTopic
                    , mkCommentText
                    )

data Comment = Comment CommentId
                       Topic
                       CommentText
                       UTCTime
               deriving Show

data Error = NoTopicInRequest
           | UnknownRoute
           | NoCommentText
           | SQLiteError SQLiteResponse

data ContentType = PlainText
                 | JSON

data ParleyRequest = AddRequest Topic CommentText
                   | ViewRequest Topic
                   | ListRequest

data DbComment =
  DbComment { dbCommentId    :: Integer
            , dbCommentTopic :: Text
            , dbCommentBody  :: Text
            , dbCommentTime  :: UTCTime
            }
            deriving Show

newtype Table = Table Text
  deriving (Show)

newtype CommentId = CommentId Integer
  deriving (Eq, Show, ToJson)

newtype Port = Port { unPort :: Int16 }
  deriving Show

newtype Topic = Topic {getTopic :: Text}
  deriving (Eq, Show)

newtype CommentText = CommentText {getComent :: Text}
  deriving (Eq, Show)

render :: ContentType -> ByteString
render PlainText = "text/plain"
render JSON      = "text/json"

mkTopic :: Text -> Either Error Topic
mkTopic "" = Left NoTopicInRequest
mkTopic t  = pure $ Topic t

mkCommentText :: Text -> Either Error CommentText
mkCommentText "" = Left NoCommentText
mkCommentText t  = pure $ CommentText t

-- instance ToJSON Comment where
--   toJSON (Comment id' topic comment time) =
--     object [ "id"      .= id'
--            , "topic"   .= topic
--            , "comment" .= comment
--            , "time"    .= time
--            ]
--
--   toEncoding (Comment id' topic comment time) =
--     pairs ( "id"      .= id
--           <> "topic"  .= topic
--           <> "coment" .= comment
--           <> "time"   .= time
--           )

app :: ParleyDb
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived

mkRequest :: Request -> IO (Either Error ParleyRequest)

handleRequest :: ParleyDb
              -> ParleyRequest
              -> IO (Either Error Response)

handleError :: Error -> Response

