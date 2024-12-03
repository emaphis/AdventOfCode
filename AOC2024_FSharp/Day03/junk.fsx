open System.IO
open System.Text.RegularExpressions

  /// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""

let input = getData "data03.txt"
let example1 = getData "test03.txt"

let example =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""

let parse input = 
    let regex = Regex("mul\((\d+),(\d+)\)")
    let m = regex.Matches(input)
    m |> Seq.map (fun mm -> (int mm.Groups.[1].Value, int mm.Groups.[2].Value)) |> Seq.toList

let numbers = example1 |> parse
numbers |> List.map (fun (a,b) -> a * b) |> List.sum