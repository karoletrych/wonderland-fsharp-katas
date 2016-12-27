// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure


type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

let playRound (card1:Card,card2:Card) =
    let suit1, rank1 = card1;
    let suit2, rank2 = card2;
    if rank1 > rank2 then
        card1
    else if rank2 > rank1 then
        card2
    else
        if suit1 > suit2 then
            card1
        else if suit2 > suit1 then
            card2
        else
            failwith "duplicated card in deck"

let playGame (hand1:Card list, hand2:Card list) =
    let rec playgameWithRoundCount (hand1:Card list, hand2:Card list) roundCount =
        if roundCount = 0 
        then 
            printfn "DRAW!" 
            []
        else        
            match (hand1, hand2) with
            | (hand1, []) -> 
                printfn "player1 won" 
                hand1
            | ([], hand2) -> 
                printfn "player2 won" 
                hand2
            | (h1_head::h1_tail, h2_head::h2_tail) -> 
            if playRound (h1_head, h2_head) = h1_head 
            then 
                playgameWithRoundCount (h1_tail @ [h1_head; h2_head], h2_tail) (roundCount - 1)
            else 
                playgameWithRoundCount (h1_tail, h2_tail @ [h1_head; h2_head]) (roundCount - 1)
    playgameWithRoundCount (hand1, hand2) 100

let suits = [ Spade; Club; Diamond; Heart ]
let heads = [ Jack; Queen; King; Ace ]

let ranks =
    [   for v in 2 .. 10 -> Value v
        for head in heads -> head
    ]

let deck = seq {
    for suit in suits do
        for rank in ranks -> suit,rank }

let split (s:Card list) = List.fold (fun (xs, ys) e -> (e::ys, xs)) ([], []) s

// fill in tests for your game

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =
    test <@playRound ((Spade, Ace), (Spade, King)) = (Spade, Ace) @>

    test <@playRound ((Spade, Queen), (Heart, Jack)) = (Spade, Queen) @>
    
    test <@playRound ((Spade, King), (Heart, Queen)) = (Spade, King) @>

    test <@playRound ((Spade, King), (Heart, Queen)) = (Spade, King) @>

    test <@playRound ((Club, King), (Spade, King)) = (Club, King) @>
  
    test <@playRound ((Diamond, King), (Club, King)) = (Diamond, King) @>

    test <@playRound ((Heart, King), (Diamond, King)) = (Heart, King) @>

    test <@ List.isEmpty (deck |> Seq.toList |> split |> playGame) @>

// run the tests
tests ()
