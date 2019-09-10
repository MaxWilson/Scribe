module Tests

open Wilson.Scribe.Transforms
open Expecto
open System

module String =
    let trimN n (input:string) =
        if (input.Length < n) then input
        else input.Substring(0, n) + "..."

let inputOutputPairs =
    [

    "\n","\n"
    "abc", "abc"
    """
    a""","""
    a"""

    "a new line b",
    """a
    b"""

    "a. new line. b",
    """a.
    b"""

    "quote I am here close quote new line. He flinched.",
    """"I am here,"
    He flinched.
    """

    "I see. Then there's really nothing I can do, is there newline? I suppose not. Newline. Farewell.",
    "I see. Then there's really nothing I can do, is there?
    I suppose not.
    Farewell."

    "The old man sat in his chair. new line. He'd been dead for several weeks. New line. quote do you think he felt anything unquote asked Timmy new line. Quote nope unquote said Bob.",
    """The old man sat in his chair.
    He'd been dead for several weeks.
    "Do you think he felt anything," asked Timmy.
    "Nope," said Bob.""";
    ] |> List.map (fun (input, output) -> input.Replace(Environment.NewLine+"    ", Environment.NewLine), output.Replace(Environment.NewLine+"    ", Environment.NewLine))

let checkTransform(input:string, expected:string) =
    test (input.Replace(Environment.NewLine, " ") |> String.trimN 12) {
        let actual = replaceKeywordsAndFixPunctionation input
        Expect.equal actual expected ("Text was not transformed correctly. Input:\n" + input)
    };

[<Tests>]
let checkGrammar =
    testList "Check grammar" (inputOutputPairs |> List.map checkTransform)
