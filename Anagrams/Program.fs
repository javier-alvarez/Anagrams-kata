// Anagrams

let isPrime (n:bigint) = 
    let bound = bigint (sqrt(float n))
    seq {bigint(2)..bound}
            |> Seq.exists (fun (x:bigint) -> n % x = bigint 0) 
            |> not
let primes =
    Seq.initInfinite (fun i -> i + 2) //need to skip 0 and 1 for isPrime
    |> Seq.map (fun i -> bigint i)
    |> Seq.filter isPrime



let alfabet = Array.append [|'a'..'z'|] [|'-'|]

let letterToPrime = Seq.zip alfabet primes |> Map.ofSeq 

let encodeAnagram (word:string) = 
    let mapping = Array.map (fun (x:char) -> Map.find x letterToPrime) (word.ToCharArray())  
    Array.reduce (fun (acc:bigint) value -> acc * value) mapping  
                                        


open System.Diagnostics
[<EntryPoint>]
let main argv = 
    
    //Read words
    let words = System.IO.File.ReadLines("words.txt")

    let stopWatch = Stopwatch.StartNew()
    let wordsToPrime = 
        Seq.map (fun (x:string) -> (x,encodeAnagram x)) words 
        |> Seq.groupBy (fun x -> snd x) 
        |> Seq.toArray
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
   

    let anagrams = 
        wordsToPrime
        |> Seq.map (fun x -> sprintf "%s %A" ((fst x).ToString()) ((Seq.map fst (snd x)) |> Seq.toArray)) 
        
    //Write results
    System.IO.File.WriteAllLines("anagrams.txt", anagrams)
    0 // return an integer exit code
    
