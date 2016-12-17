// See the file alphabet-cipher.md for detailed information.
open System

type Message = string
type Keyword = string

let lengthenKey (key:string) totalLength =
    let numberOfRepeatedKeys = totalLength/key.Length + 1
    let overlengthenedKey = String.replicate numberOfRepeatedKeys key
    overlengthenedKey.[..totalLength-1]
let chart:char[,] = Array2D.init 26 26 (fun x y -> (char) (((x+y)%26)+97) )

let encodeChar (x:char) (y:char) = chart.[(int)x-97,(int)y-97]

let rec countRepeatingPatterns (where :string) (what : string) =
    let length = what.Length-1
    let beginning = where.[0..if length<where.Length then length else where.Length-1]
    if  beginning = what then
        1 + countRepeatingPatterns (where.Substring (if length<where.Length then what.Length else where.Length-1)) what
    else 
        0

let decodeChar (messageChar:char) (keyChar:char) = 
    let row = chart.[(int)keyChar-97,*]
    let decoded = 
        row 
        |> String.Concat 
        |> (fun x -> x.IndexOf(messageChar)) 
        |> (fun x -> (char) (x + 97))
    decoded

let findRepeatedWord(repeated : string) = 
    let countRepetitionsInRepeated = countRepeatingPatterns repeated
    let possibleSubstringsWithRepetitionsCount = 
        [1..repeated.Length] 
        |> List.map
               ((fun length -> repeated.[..length-1]) 
                >> (fun substring -> (substring, countRepetitionsInRepeated substring)))
    possibleSubstringsWithRepetitionsCount
        |> List.maxBy snd

let encode (key:Keyword) (message:Message) : Message =
    let lenghthenedKey = lengthenKey key message.Length
    (Seq.map2 encodeChar message lenghthenedKey) |> String.Concat

let decode (key:Keyword) (message:Message) : Message =
    let lenghthenedKey = lengthenKey key message.Length
    (Seq.map2 decodeChar message lenghthenedKey) |> String.Concat

let decipher (cipher:Message) (message:Message) : Keyword =
    let repeatedKey = (Seq.map2 decodeChar cipher message) |> String.Concat
    let (key, repetitions) = findRepeatedWord repeatedKey
    key

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =
    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // // // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
