
let rec gcd = function
    | (a, 0) -> a
    | (a, b) -> gcd(b, a % b);;

let (.+) (a,b) (c,d) = 
    match gcd((a*d + b*c, b*d)) with
        | 1 -> a*d + b*c, b*d //GCD=1, result stays as is
        | _ -> (a*d + b*c)/gcd(a*d + b*c, b*d), b*d / gcd(a*d + b*c, b*d);;

let (.*) (a,b) (c,d) =
    match gcd(a*c, b*d) with
        | 1 -> a*c, b*d //GCD=1, result stays as is
        | _ -> a*c / gcd(a*c, b*d), b*d / gcd(a*c, b*d);;

let revlists xs = List.map List.rev xs;;


let rec interleave(xs, ys) = 
    match xs, ys with
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x :: y :: interleave(xs, ys);;
//noticed same as: let rec interleave = function ..., which is "better"?


let gencut n xs =
    //needed aux function to help build 'ys' in the new, split tuple : (ys, xs)
    let rec aux ys = function//starting out with ys = [], where n and xs are passed from gencut
        //only have to match against n, and xs
        | 0, xs         -> List.rev ys, xs 
        | _, []         -> List.rev ys, [] //hit the end of xs before n
        | n, x::xs      -> aux (x::ys) (n-1, xs)
    aux [] (n, xs);;
let cut xs = gencut (List.length xs / 2) xs;;


let shuffle xs = interleave(cut(xs));;

let countshuffles x = 
    let rec countaux (deck, tar) = 
        if deck = tar then 1
        else countaux (shuffle deck, tar) + 1
    countaux(shuffle [1..x], [1..x]);;