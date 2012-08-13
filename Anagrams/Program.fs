// Anagrams

let isPrime n = 
    let bound = int (sqrt(float n))
    seq{2..bound}
            |> Seq.exists (fun x -> n % x = 0) 
            |> not
let rec nextPrime n = 
    if isPrime (n + 1) then n + 1
    else nextPrime (n+1)
let seqPrimes = 
    Seq.unfold(fun n -> Some(n, nextPrime n)) 2



let alfabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'-']

let letterToPrime = Seq.zip alfabet seqPrimes |> Map.ofSeq 

let encodeAnagram (word:string) = 
    let mapping=Array.map (fun (x:char) -> bigint (Map.find x letterToPrime)) (word.ToCharArray())  
    Array.reduce (fun (acc:bigint) value -> acc * value) mapping  
                                        


open System.Diagnostics
[<EntryPoint>]
let main argv = 
    
    //Read words
    let words = System.IO.File.ReadLines("words.txt")

    let stopWatch = Stopwatch.StartNew()
    let wordsToPrime = Seq.map (fun (x:string) -> (x,encodeAnagram x)) words |> Seq.groupBy (fun x -> snd x) |> Seq.toArray
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
   

    let anagrams = wordsToPrime|> Seq.map (fun x -> sprintf "%s %A" ((fst x).ToString()) ((Seq.map fst (snd x)) |> Seq.toArray)) |> Seq.toArray
    
    
    //Write results
    System.IO.File.WriteAllLines("anagrams.txt", anagrams)
    0 // return an integer exit code
    
