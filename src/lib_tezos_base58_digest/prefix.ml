(* 32 *)
let block_hash = "\001\052" (* B(51) *)

let operation_hash = "\005\116" (* o(51) *)

let operation_list_hash = "\133\233" (* Lo(52) *)

let operation_list_list_hash = "\029\159\109" (* LLo(53) *)

let protocol_hash = "\002\170" (* P(51) *)

let context_hash = "\079\199" (* Co(52) *)

let block_metadata_hash = "\234\249" (* bm(52) *)

let operation_metadata_hash = "\005\183" (* r(51) *)

let operation_metadata_list_hash = "\134\039" (* Lr(52) *)

let operation_metadata_list_list_hash = "\029\159\182" (* LLr(53) *)

(* 20 *)
let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

let secp256k1_public_key_hash = "\006\161\161" (* tz2(36) *)

let p256_public_key_hash = "\006\161\164" (* tz3(36) *)

(* 16 *)
let cryptobox_public_key_hash = "\153\103" (* id(30) *)

(* 32 *)
let ed25519_seed = "\013\015\058\007" (* edsk(54) *)

let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

let secp256k1_secret_key = "\017\162\224\201" (* spsk(54) *)

let p256_secret_key = "\016\081\238\189" (* p2sk(54) *)

(* 56 *)
let ed25519_encrypted_seed = "\007\090\060\179\041" (* edesk(88) *)

let secp256k1_encrypted_secret_key = "\009\237\241\174\150" (* spesk(88) *)

let p256_encrypted_secret_key = "\009\048\057\115\171" (* p2esk(88) *)

(* 60 *)
let secp256k1_encrypted_scalar = "\001\131\036\086\248" (* seesk(93) *)

(* 33 *)
let secp256k1_public_key = "\003\254\226\086" (* sppk(55) *)

let p256_public_key = "\003\178\139\127" (* p2pk(55) *)

let secp256k1_scalar = "\038\248\136" (* SSp(53) *)

let secp256k1_element = "\005\092\000" (* GSp(54) *)

(* 64 *)
let ed25519_secret_key = "\043\246\078\007" (* edsk(98) *)

let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

let secp256k1_signature = "\013\115\101\019\063" (* spsig1(99) *)

let p256_signature = "\054\240\044\052" (* p2sig(98) *)

let generic_signature = "\004\130\043" (* sig(96) *)

(* 4 *)
let chain_id = "\087\082\000" (* Net(15) *)

(* 169 *)
let sapling_spending_key = "\011\237\020\092" (* sask(241) *)

(* 43 *)
let sapling_address = "\018\071\040\223" (* zet1(69) *)

let script_expr_hash =
  (* Taken from src/proto_006_PsCARTHA/lib_protocol/script_expr_hash.ml *)
  (* expr(54) *)
  "\013\044\064\027"

let contract_hash =
  (* src/proto_006_PsCARTHA/lib_protocol/contract_hash.ml KT1(36) *)
  "\002\090\121"
