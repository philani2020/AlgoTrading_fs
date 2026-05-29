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
let runAnalysis (ticker: string) =
    printfn "============================================="
    printfn " Analyzing Ticker: %s" ticker
    printfn "============================================="

    // Fetch 2 years of daily data to have enough history for a 200 SMA
    let history = fetchSkenderQuotes ticker "2y"

    // Calculate Indicators using Skender C# Extension Methods natively in F#
    let sma50Results  = history.GetSma(50)  |> Seq.toList
    let sma200Results = history.GetSma(200) |> Seq.toList
    let rsiResults    = history.GetRsi(14)  |> Seq.toList

    // Zip historical indicators together by matching index/dates
    // We parse from the tail end to look at the most recent market sessions
    let totalElements = history.Length
    printfn "Successfully loaded %d daily bars." totalElements
    printfn "---------------------------------------------\n"

    printfn "%-12s | %-10s | %-10s | %-10s | %-6s" "Date" "Close" "50 SMA" "200 SMA" "RSI"
    printfn "--------------------------------------------------------"

    // Display the last 5 trading days with their computed indicators
    for i in (totalElements - 5) .. (totalElements - 1) do
        let quote  = history.[i]
        let sma50  = sma50Results.[i]
        let sma200 = sma200Results.[i]
        let rsi    = rsiResults.[i]
        
        // Skender returns nullable types or specific Result models depending on if periods are warm yet
        let s50Str  = if sma50.Sma.HasValue  then sprintf "%8.2f" sma50.Sma.Value  else "Warming Up"
        let s200Str = if sma200.Sma.HasValue then sprintf "%8.2f" sma200.Sma.Value else "Warming Up"
        let rsiStr  = if rsi.Rsi.HasValue    then sprintf "%6.1f" rsi.Rsi.Value    else "N/A"

        printfn "%s | %10.2f | %s | %s | %s" 
            (quote.Date.ToString("yyyy-MM-dd")) 
            quote.Close 
            s50Str 
            s200Str 
            rsiStr

    // Determine current market regime state
    printfn "\n---------------------------------------------"
    let latest50  = sma50Results |> List.last
    let latest200 = sma200Results |> List.last
    
    if latest50.Sma.HasValue && latest200.Sma.HasValue then
        let diff = latest50.Sma.Value - latest200.Sma.Value
        if diff > 0 then 
            printfn "Regime State: Bullish Trend (50 SMA is %.2f above 200 SMA)" diff
        else 
            printfn "Regime State: Bearish Trend (50 SMA is %.2f below 200 SMA)" (abs diff)
    else
        printfn "Regime State: Insufficient history to establish crossover regime state."
    printfn "=============================================\n"

// Run execution against a liquid tech name
let tickers = ["SHP.JO";"MTN.JO";"SOL.JO"]
tickers |> Seq.iter runAnalysis

