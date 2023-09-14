module Parser (
  ParseError,
  Offset,
  parseTokens
) where

import Tokenizer

data Expression
    = FunctionCall Expression [Expression]
    | IdentifierExpr String
    | NumberLiteral Int
    | BooleanLiteralExpr Bool
    | LetExpression [(String, Expression)] [Expression]
    deriving (Show)

type ParseError = (String, Offset)

parseTokens :: [Token] -> ([Expression], [ParseError])
parseTokens tokens = parseExpressions tokens

parseExpressions :: [Token] -> ([Expression], [ParseError])
parseExpressions tokens = go tokens [] []
  where
    go :: [Token] -> [Expression] -> [ParseError] -> ([Expression], [ParseError])
    go [] exprs errs = (reverse exprs, errs)
    go (t:ts) exprs errs =
      case parseExpression (t:ts) of
        (Just expr, remainingTokens, newErrs) -> go remainingTokens (expr:exprs) (errs ++ newErrs)
        (Nothing, remainingTokens, newErrs) -> go remainingTokens exprs (errs ++ newErrs)


parseExpression :: [Token] -> (Maybe Expression, [Token], [ParseError])
parseExpression [] = (Nothing, [], [])
parseExpression [EOF] = (Nothing, [], [])
parseExpression (t:tokens) =
    case t of
        OpenParen offset -> parseFunctionCall tokens offset
        Identifier str offset -> (Just $ IdentifierExpr str, tokens, [])
        IntegerLiteral int offset -> (Just $ NumberLiteral int, tokens, [])
        BooleanLiteral b offset -> (Just $ BooleanLiteralExpr b, tokens, [])
        _ -> (Nothing, tokens, [(show t ++ " unexpected at this position", getTokenOffset t)])

parseFunctionCall :: [Token] -> Offset -> (Maybe Expression, [Token], [ParseError])
parseFunctionCall [] openParenOffset =
    (Nothing, [], [("Unclosed function call starting at " ++ show openParenOffset, openParenOffset)])
parseFunctionCall (t:tokens) openParenOffset =
    case t of
        LetKeyword offset -> parseLet tokens offset
        IntegerLiteral int offset ->
            (Nothing, tokens, [("Integer literals cannot be used as functions", offset)])
        BooleanLiteral b offset ->
            (Nothing, tokens, [("Boolean literals cannot be used as functions", offset)])
        _ ->
            let (funcExpr, remainingTokensAfterFunc, errorsInFunc) = parseExpression (t:tokens)
            in case funcExpr of
                Just func ->
                    let (args, remainingTokens, errorsInArgs) = parseFunctionArgs remainingTokensAfterFunc [] openParenOffset
                    in (Just (FunctionCall func args), remainingTokens, errorsInFunc ++ errorsInArgs)
                Nothing ->
                    (Nothing, remainingTokensAfterFunc, [("Expected a function identifier or function call after (, starting at " ++ show openParenOffset, openParenOffset)] ++ errorsInFunc)

parseLet :: [Token] -> Offset -> (Maybe Expression, [Token], [ParseError])
parseLet [] oldOffset = (Nothing, [], [("Unclosed let expression " ++ show oldOffset, oldOffset)])
parseLet (t:tokens) oldOffset =
    case t of
        OpenParen offset ->
          let
            (bindings, remainingTokens, errors) = parseLetBindings (t:tokens) oldOffset-- corrected the function call
            (exprs, tks, args_errors) = parseFunctionArgs remainingTokens [] oldOffset
          in (Just $ LetExpression bindings exprs, tks, args_errors ++ errors)
        _ -> (Nothing, tokens, [("Expected arguments", (mergeOffsets oldOffset $ getTokenOffset t))]) -- changed to correct data constructor

parseLetBindings :: [Token] -> Offset -> ([(String, Expression)], [Token], [ParseError])
parseLetBindings [] oldOffset = ([], [], [("Unclosed let expression " ++ show oldOffset, oldOffset)])
parseLetBindings [_] oldOffset = ([], [], [("Unclosed let expression " ++ show oldOffset, oldOffset)])
parseLetBindings (t:t2:tokens) oldOffset =
    case (t, t2) of
        (CloseParen o1, _) -> ([], t2:tokens, [])
        (OpenParen o1, OpenParen o2) ->
            let (maybeExpr, remainingTokens, errors) = parseLetBinding tokens oldOffset
            in case maybeExpr of
                Just expr ->
                    let (restExpr, restTokens, restErrors) = parseLetBindings remainingTokens oldOffset
                    in (expr : restExpr, restTokens, errors ++ restErrors)
                Nothing -> ([], remainingTokens, errors)
        (OpenParen o1, _) ->
            let (maybeExpr, remainingTokens, errors) = parseLetBinding (t2:tokens) oldOffset
            in case maybeExpr of
                Just expr -> ([(expr)], remainingTokens, errors)
                Nothing -> ([], remainingTokens, errors)
        _ -> ([], tokens, [("Expected arguments", (mergeOffsets oldOffset $ getTokenOffset t))]) -- changed to correct data constructor and added undefined for offset (you should replace undefined with the actual value)

parseLetBinding :: [Token] -> Offset -> (Maybe (String, Expression), [Token], [ParseError])
parseLetBinding [] oldOffset = (Nothing, [], [("Unclosed let expression " ++ show oldOffset, oldOffset)])
parseLetBinding tokens oldOffset =
    case tokens of
        [] -> (Nothing, [], [("Unexpected end of tokens", oldOffset)]) -- changed to correct data constructor
        (t:ts) ->
            case t of
                Identifier name offset ->
                    let (maybeExpr, remainingTokens, errors) = parseExpression ts
                    in case maybeExpr of
                        Just expr -> case remainingTokens of
                                (CloseParen _):rest -> (Just (name, expr), rest, errors)
                                r:_ -> (Nothing, remainingTokens, ("Non-Closing let binding", mergeOffsets oldOffset $ getTokenOffset r):errors)
                                _ -> (Nothing, remainingTokens, ("Non-Closing let binding", mergeOffsets oldOffset offset):errors)
                        Nothing -> (Nothing, remainingTokens, errors)
                _ ->
                    (Nothing, ts, [(("Expected identifier for varbinding: " ++ show t), getTokenOffset t) ]) -- changed to correct data constructor



parseFunctionArgs :: [Token] -> [Expression] -> Offset -> ([Expression], [Token], [ParseError])
parseFunctionArgs tokens acc startOffset =
    case tokens of
        (CloseParen offset):rest -> (acc, rest, [])
        [] -> (acc, [], [("Unclosed function call from " ++ show startOffset ++ " to end", startOffset)])
        (EOF):rest -> (acc, [], [("Unclosed function call from " ++ show startOffset ++ " to end", startOffset)])
        _ ->
            let (arg, remainingTokens, errors) = parseExpression tokens
            in case arg of
                Just a -> parseFunctionArgs remainingTokens (acc ++ [a]) startOffset
                Nothing -> (acc, remainingTokens, errors ++ [("Expected an argument from " ++ show startOffset, startOffset)])

getTokenOffset :: Token -> Offset
getTokenOffset (OpenParen offset) = offset
getTokenOffset (CloseParen offset) = offset
getTokenOffset (Tokenizer.Identifier _ offset) = offset
getTokenOffset (IntegerLiteral _ offset) = offset
getTokenOffset (Tokenizer.BooleanLiteral _ offset) = offset
getTokenOffset (LetKeyword offset) = offset
getTokenOffset (InvalidToken _ offset) = offset
getTokenOffset (EOF) = (0, 0)

mergeOffsets :: Offset -> Offset -> Offset
mergeOffsets (start1, end1) (start2, end2) = (min start1 start2, max end1 end2)