#I """bin\Debug"""

#load "Transforms.fs"

open Wilson.Scribe.Transforms
open System

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
        | OWS(AnyCase "newline" (Punctuation(punc, OWS rest))) ->
            Some(Some punc, rest)
        | OWS(AnyCase "newline" rest) ->
            Some(None, rest)
        | OWS(AnyCase "new" (OWS (AnyCase "line" (Punctuation(punc, OWS rest))))) ->
            Some(Some punc, rest)
        | OWS(AnyCase "new" (OWS (AnyCase "line" (OWS rest)))) ->
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
        | OWS(Punctuation(punc, OWS(NewLine(_, OWS rest)))) -> punc::helper(newLine::rest)
        | OWS(NewLine(Some punc, rest)) -> punc::newLine::helper(rest)
        | OWS(NewLine(None, rest)) -> newLine::helper(rest)
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

replaceKeywordsAndFixPunctionation "a. new line. b" |> printfn "%A"
replaceKeywordsAndFixPunctionation "a new line b" |> printfn "%A"
replaceKeywordsAndFixPunctionation "close quote" |> printfn "%s"
replaceKeywordsAndFixPunctionation "quote I am here close quote, said max" |> printfn "%s"
replaceKeywordsAndFixPunctionation "quote I am here close quote new line, said max" |> printfn "%s"
replaceKeywordsAndFixPunctionation "quote I am here close quote, said max. New line she shrugged" |> printfn "%s"
replaceKeywordsAndFixPunctionation "The old man sat in his chair. new line. He'd been dead for several weeks. New line. quote do you think he felt anything unquote asked Timmy new line. Quote nope unquote said Bob." |> printfn "%s"
let t = "Old man sat in his chair newline, he'd been dead for several weeks new line. Quote do you think he felt anything unquote? Asked me new line? Quote Nope Unquote said Bob. ."
replaceKeywordsAndFixPunctionation t |> printfn "%s"

