module Tests
open Expecto

[<Tests>]
let aTest =
  test "toEmail with bob gives bob [at] acme [dot] com" {
      let name = "bob"
      let expected = "bob@acme.com"
      let actual = id name
      Expect.equal actual expected "emails did not match"
  }

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
