module Wilson.Scribe.Transforms
open System

let tokenize (str: string) =
    // returns: text * index of first unconsumed input, if any
    let consume pred startIx =
        let rec help ix =
            if ix >= str.Length then str.Substring(startIx), ix
            elif pred str.[ix] then help (ix+1)
            else str.Substring(startIx, ix - startIx), ix
        help startIx
    let rec helper startIx =
        if startIx >= str.Length then
            []
        elif System.Char.IsWhiteSpace str.[startIx] then
            let txt, ix = consume System.Char.IsWhiteSpace startIx
            txt :: (helper ix)
        elif System.Char.IsLetterOrDigit str.[startIx] then
            let txt, ix = consume System.Char.IsLetterOrDigit startIx
            txt :: (helper ix)
        else
            let txt, ix = str.[startIx].ToString(), startIx+1
            txt :: (helper ix)
    helper 0

let replaceKeywordsAndFixPunctionation str =
    let tokens = tokenize str
    let (|AnyCase|_|) (pattern: string) = function
        | input :: rest when System.String.Equals(pattern, input, System.StringComparison.InvariantCultureIgnoreCase) ->
                Some rest
        | _ -> None
    let (|OWS|) =
        let rec help = function
            | input :: rest when System.String.IsNullOrWhiteSpace input ->
                help rest
            | rest -> rest
        help
    let (|WS|) =
        let rec help accum = function
            | input :: rest when System.String.IsNullOrWhiteSpace input ->
                help (input::accum) rest
            | rest ->
                match accum with
                | [] -> None
                | _ -> Some(System.String.Join("", accum), rest)
        help []
    let (|Word|_|) = function
        | (input:string)::rest when input.Length > 0 && System.Char.IsLetterOrDigit input.[0] ->
            Some(input, rest)
        | _ -> None
    let (|Punctuation|_|) = function
        | input::rest ->
            match input with
            | "." | "!" | "?" | "," ->
                Some(input, rest)
            | _ -> None
        | _ -> None
    let (|NewLine|_|) = function
        | AnyCase "newline" (Punctuation(punc, rest)) ->
            Some(Some punc, rest)
        | AnyCase "newline" rest ->
            Some(None, rest)
        | AnyCase "new" (OWS (AnyCase "line" (Punctuation(punc, rest)))) ->
            Some(Some punc, rest)
        | AnyCase "new" (OWS (AnyCase "line" rest)) ->
            Some(None, rest)
        | _ -> None
    let (|OpenQuote|_|) = function
        | AnyCase "open" (OWS (AnyCase "quote" rest)) -> Some rest
        | AnyCase "open" (OWS (AnyCase "quotes" rest)) -> Some rest
        | AnyCase "quote" rest -> Some rest
        | _ -> None
    let (|CloseQuote|_|) = function
        | AnyCase "close" (OWS (AnyCase "quote" rest)) -> Some rest
        | AnyCase "close" (OWS (AnyCase "quotes" rest)) -> Some rest
        | AnyCase "unquote" rest -> Some rest
        | _ -> None
    let capitalize (word:string) =
        if word.Length < 2 then word.ToUpper()
        else
            word.Substring(0, 1).ToUpperInvariant() + word.Substring(1, word.Length - 1)
    let newLine = Environment.NewLine
    let rec helper = function
        | Punctuation(punc, OWS (CloseQuote (OWS (NewLine(_, (Word(word, rest))))))) -> punc::"\""::newLine::(helper (capitalize word::rest))
        | Punctuation(punc, OWS (CloseQuote rest)) -> punc::"\""::(helper rest)
        | OWS(NewLine(Some punc, rest)) -> helper(punc::newLine::rest)
        | Word(word, OWS (CloseQuote rest)) -> word::","::"\""::helper rest
        | OWS (CloseQuote (OWS (NewLine(Some punc, OWS (Word(word, rest)))))) -> punc::"\""::newLine::(helper (capitalize word::rest))
        | OWS (CloseQuote (OWS (NewLine(None, OWS (Word(word, rest)))))) -> "\""::newLine::(helper (capitalize word::rest))
        | OWS (CloseQuote (OWS (NewLine(Some punc, rest)))) -> punc::"\""::newLine::(helper rest)
        | OWS (CloseQuote (OWS (NewLine(None, rest)))) -> "\""::newLine::(helper rest)
        | OWS (CloseQuote (OWS (Punctuation(punc, rest)))) -> punc::"\""::(helper rest)
        | OWS (CloseQuote rest) -> "\""::(helper rest)
        | OWS (CloseQuote (OWS (Punctuation(punc, OWS (NewLine(_, (Word(word, rest)))))))) -> punc::"\""::newLine::(helper (capitalize word::rest))
        | OWS (CloseQuote (OWS (NewLine(Some punc, OWS (Word(word, rest)))))) -> punc::"\""::newLine::(helper (capitalize word::rest))
        | OWS (CloseQuote (OWS (NewLine(None, OWS (Word(word, rest)))))) -> "\""::newLine::(helper (capitalize word::rest))
        | OWS (CloseQuote (OWS (NewLine(Some punc, rest)))) -> punc::"\""::newLine::(helper rest)
        | OWS (CloseQuote (OWS (NewLine(None, rest)))) -> "\""::newLine::(helper rest)
        | OWS (CloseQuote (OWS (Punctuation(punc, rest)))) -> punc::"\""::(helper rest)
        | OWS (CloseQuote rest) -> "\""::(helper rest)
        | Word (word, OWS (NewLine (Some punc, OWS rest))) -> word::punc::newLine::helper rest
        | OWS (NewLine (_, OWS rest)) -> newLine::helper rest
        | OpenQuote (OWS (Word(word, rest))) -> "\""::(helper (capitalize word::rest))
        | OpenQuote (OWS rest) -> "\""::(helper rest)
        | h::rest -> h::(helper rest)
        | [] -> []
    System.String.Join("", tokens |> helper)