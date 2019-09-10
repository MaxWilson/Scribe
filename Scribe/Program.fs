// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


module Wilson.Scribe
open Microsoft.CognitiveServices
open Microsoft.CognitiveServices.Speech
open Microsoft.CognitiveServices.Speech.Audio
open System.Text.RegularExpressions
open Scribe.Transforms

let config =  SpeechConfig.FromSubscription("0269707aff2743f9b1d80526928ecfbd", "westus"); // TODO: replace with a real key config--this one is a free, temporary key

type FileReader(filePath) =
    inherit PullAudioInputStreamCallback()
    //let f = System.IO.BinaryReader(new System.IO.File.ReadAllBytes(filePath))
    let f = new System.IO.BinaryReader(System.IO.File.OpenRead(filePath))
    override this.Read(buffer, size:uint32) =
        f.Read(buffer, 0, int size)
    override this.Close() = f.Close()

module Convert =
    open System.IO
    open NAudio.Wave
    let mp3ToWav (fileName: string) =
        use reader = new Mp3FileReader(File.OpenRead fileName)
        use pcm = WaveFormatConversionStream.CreatePcmStream(reader)
        let outPath = Path.ChangeExtension(Path.GetTempFileName(), "wav")
        WaveFileWriter.CreateWaveFile(outPath, pcm)
        outPath

let transcribe (filePath: string) =
    let mutable finished = false
    let mutable txt = []
    printfn "Transcribing..."
    use r2 = if filePath.EndsWith ".wav" then
                new SpeechRecognizer(config, AudioConfig.FromWavFileInput filePath)
             else
                let wavPath = Convert.mp3ToWav filePath
                new SpeechRecognizer(config, AudioConfig.FromWavFileInput wavPath)
    ignore <| r2.Recognized.Subscribe(fun e -> printfn "Transcribing... %s" e.Result.Text; if (not <| System.String.IsNullOrWhiteSpace e.Result.Text) then txt <- e.Result.Text :: txt)
    ignore <| r2.SessionStopped.Subscribe(fun e -> finished <- true; r2.StopContinuousRecognitionAsync() |> ignore)
    ignore <| r2.StartContinuousRecognitionAsync()
    while finished = false do
        System.Threading.Thread.Sleep(1000)
    System.String.Join(" ", List.rev txt |> List.map (fun x -> x.Trim()) |> List.filter(not << System.String.IsNullOrWhiteSpace))
let transcribeLive pretransform =
    let mutable finished = false
    let mutable txt = []
    use r2 = new SpeechRecognizer(config)
    ignore <| r2.Recognized.Subscribe(fun e ->
        let txt' =
            match e.Result.Text with
            | txt when Regex.IsMatch(txt, "stop[.]? listening[.]?$", RegexOptions.IgnoreCase) ->
                finished <- true;
                r2.StopContinuousRecognitionAsync().ContinueWith(fun _ -> printfn "Stopped") |> ignore
                Regex.Replace(txt, "stop listening", "", RegexOptions.IgnoreCase)
            | txt -> txt
        if (not <| System.String.IsNullOrWhiteSpace txt') then
            printfn "Transcribing... %s" <| pretransform txt';
            txt <- txt' :: txt
    )
    ignore <| r2.SessionStopped.Subscribe(fun e -> finished <- true; r2.StopContinuousRecognitionAsync() |> ignore)
    ignore <| r2.StartContinuousRecognitionAsync()
    printfn "Transcribing..."
    while finished = false do
        System.Threading.Thread.Sleep(1000)
    System.String.Join(" ", List.rev txt |> List.map (fun x -> x.Trim()) |> List.filter(not << System.String.IsNullOrWhiteSpace))


// CURRENT STATE: linebreaks need to count as whitespace so they play nicely with punctuation fixups.
// CURRENT:
//    ". I miss you." said Max. Jenny smiled. Call Dino", she said. "I know" she said.
//    . ". Are you here right now" he said
//    ? "Yep.."

#if INTERACTIVE
#else
[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> transcribeLive replaceKeywordsAndFixPunctionation |> replaceKeywordsAndFixPunctionation |> printfn "%s"
    | [|filePath|] -> transcribe filePath |> replaceKeywordsAndFixPunctionation |> printfn "%s"
    | _ -> printfn "Usage: Scribe [<filePath>]\nSupported formats: .wav and .mp4"
    0 // return an integer exit code
#endif