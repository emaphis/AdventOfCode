open System.IO
open System.Text.RegularExpressions

  /// Get data from the `..\data` directory and store it in a string
  /// since there is only one item in the data file
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""

let input = getData "data03.txt"
let example1 = getData "test03.txt"

// Since Part 2 is a language with a simple syntax let's turn this into a simple interpreter.
/// Syntax tokens for the interpreter.


let rx =
            Regex(@"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)", RegexOptions.Compiled ||| RegexOptions.Multiline)


let matches = rx.Matches(example1)

printfn "%A" matches