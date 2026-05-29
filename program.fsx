#r "nuget: FSharp.Data"
#r "nuget: Skender.Stock.Indicators"

open System
open FSharp.Data
open Skender.Stock.Indicators
open FSharp.Core

// -------------------------------------------------------------------
// 1. Setup Yahoo Finance API Types using F# JSON Type Provider
// -------------------------------------------------------------------
// We use a sample URL to bake the type system shapes at compile-time
type YahooFinanceApi = JsonProvider<"https://query1.finance.yahoo.com/v8/finance/chart/AAPL?interval=1d&range=5d">



/// Fetches raw historical market data and maps it cleanly to Skender Quotes
let fetchSkenderQuotes (ticker: string) (range: string) : Quote list =
    let url = sprintf "https://query1.finance.yahoo.com/v8/finance/chart/%s?interval=1d&range=%s" ticker range
    let feed = YahooFinanceApi.Load(url)
    
    let result = feed.Chart.Result.[0]
    let timestamps = result.Timestamp
    let indicators = result.Indicators.Quote.[0]
    printfn "============================================="
    printfn " Short Name: %s" result.Meta.ShortName
    printfn "============================================="
    timestamps
    |> Seq.mapi (fun i t ->
        match indicators.Open.[i], indicators.High.[i], indicators.Low.[i], indicators.Close.[i], indicators.Volume.[i] with
        | o, h, l, c, v ->
            let q = Quote()
            q.Date <- DateTimeOffset.FromUnixTimeSeconds(t).UtcDateTime
            q.Open <- o
            q.High <- h
            q.Low <- l
            q.Close <- c
            q.Volume <- Convert.ToDecimal(v)
            Some q
        | _-> None
    )
    |> Seq.choose id
    |> Seq.toList

// -------------------------------------------------------------------
// 2. Main Strategy Logic
// -------------------------------------------------------------------

let sma50IsBelowSma200 (sma50: float) (sma200: float) =
    sma50 < sma200
    
let withinSpecifiedRange (sma50: float) (sma200: float) =
    abs (sma200 - sma50 ) / sma200 <= 0.05

let isBullish (sma50: float[]) (sma200: float[]) =
    Seq.forall2 sma50IsBelowSma200 sma50 sma200
    
let unWrapValue (result: SmaResult) =
    (result.Sma |> Option.ofNullable).Value
            


let runAnalysis (ticker: string) =
    printfn "============================================="
    printfn " Analyzing Ticker: %s" ticker
    printfn "============================================="

    // Fetch 2 years of daily data to have enough history for a 200 SMA
    let history = fetchSkenderQuotes ticker "2y"

    // Calculate Indicators using Skender C# Extension Methods natively in F#
    let sma50Results  = history.GetSma(50)  
                        |> Seq.toArray

    let sma200Results = history.GetSma(200) 
                                |> Seq.toArray

    // let volume20 = history.GetVwma 20                                             
    let last5Sma50 = sma50Results 
                    |> Array.skip (sma50Results.Length - 5)
                    |> Array.map unWrapValue
    let last5Sma200 = sma200Results 
                    |> Array.skip (sma200Results.Length - 5)
                    |> Array.map unWrapValue

    let rsiResults    = history.GetRsi(14)  |> Seq.toArray

    let latest50= sma50Results |> Array.last |> unWrapValue
    let latest200 = sma200Results |> Array.last |> unWrapValue
    let latestRsi = rsiResults |> Array.last

    
    let withinRange = withinSpecifiedRange   latest50 latest200
    let bullish = isBullish last5Sma50 last5Sma200
    
    if withinRange && bullish then  printfn "Ticker: %s is bullish" ticker
    else printfn "Ticker: %s is bearish" ticker

    
let tickers = ["SHP.JO";"MTN.JO";"SOL.JO"]
tickers |> Seq.iter runAnalysis

