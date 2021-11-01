module String = struct
  let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
  let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))

  let blake2b ~size x =
    let blake2b32 = Digestif.blake2b size in
    Digestif.digest_string blake2b32 x |> Digestif.to_raw_string blake2b32

  let%expect_test _ =
    let phash f v =
      Printf.printf "%S -> " v ;
      String.iter (fun c -> Printf.printf "%X" (int_of_char c)) (f v) ;
      Printf.printf "\n%!" in
    (* Can double-checked with a shell:
       $ printf hello | sha256sum
       2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824  -
       $ printf hello | sha512sum
       9b71d224bd62f3785d96d46ad3ea3d73319bfbc2890caadae2dff72519673ca72323c3d99ba5c11d7c7acc6e14b8c5da0c4663475c2e5c3adef46f73bcdec043  -
    *)
    phash sha256 "hello" ;
    [%expect
      {|
      "hello" -> 2CF24DBA5FB0A3E26E83B2AC5B9E29E1B161E5C1FA7425E7343362938B9824
      |}] ;
    phash sha512 "hello" ;
    [%expect
      {|
      "hello" -> 9B71D224BD62F3785D96D46AD3EA3D73319BFBC289CAADAE2DFF72519673CA72323C3D99BA5C11D7C7ACC6E14B8C5DAC4663475C2E5C3ADEF46F73BCDEC043
      |}] ;
    phash (blake2b ~size:32) "hello" ;
    [%expect
      {|
      "hello" -> 324DCF27DD4A3A932C441F365A25E86B173DEFA4B8E58948253471B81B72CF
      |}] ;
    let all = [sha256; sha512; blake2b ~size:32] in
    List.iter (fun f -> phash f "") all ;
    [%expect
      {|
      "" -> E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855
      "" -> CF83E1357EEFB8BDF1542850D66D807D620E45B5715DC83F4A921D36CE9CE47D0D13C5D85F2B0FF8318D2877EEC2F63B931BD47417A81A538327AF927DA3E
      "" -> E5751C026E543B2E8AB2EB06099DAA1D1E5DF47778F7787FAAB45CDF12FE3A8
      |}] ;
    ()
end
