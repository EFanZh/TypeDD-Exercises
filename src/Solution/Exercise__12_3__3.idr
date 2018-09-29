%default total

record Votes where
    constructor MkVotes
    upvotes : Integer
    downvotes : Integer

record Article where
    constructor MkArticle
    title : String
    url : String
    score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

------------------------------------------------------------------------------------------------------------------------

getScore : Article -> Integer
getScore article = case score article of
                       (MkVotes upvotes downvotes) => upvotes - downvotes

------------------------------------------------------------------------------------------------------------------------

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)
