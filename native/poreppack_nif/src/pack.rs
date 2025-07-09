use sha2::{Sha256, Digest};
use rustler::{Binary, OwnedBinary, Encoder, Env, NifResult, Term};

rustler::atoms! {
    ok,
    error,
    invalid_input_size
}

#[rustler::nif(schedule = "DirtyCpu")]
fn sha256_hash<'a>(env: Env<'a>, data: Binary<'a>, miner: Binary<'a>) -> NifResult<Term<'a>> {
    let mut hasher = Sha256::new();
    let mut first_input: [u8; 64] = [0; 64];
    let mut buffer1 : [u8; 32] = [0; 32];
    let mut buffer2 : [u8; 32] = [0; 32];

    if data.len() != 32 || miner.len() != 32 {
        return Ok((error(), invalid_input_size()).encode(env))
    }

    buffer1.copy_from_slice(data.as_slice());
    buffer2.copy_from_slice(miner.as_slice());
    
    first_input[0..32].copy_from_slice(buffer1.as_slice());
    first_input[32..64].copy_from_slice(miner.as_slice());

    hasher.update(first_input.as_ref());
    hasher.finalize_into_reset((&mut buffer1).into());

    for n in 1..1000000 {
        if n % 2 == 1 {
            hasher.update(buffer1.as_ref());
            hasher.finalize_into_reset((&mut buffer2).into());
        } else {
            hasher.update(buffer2.as_ref());
            hasher.finalize_into_reset((&mut buffer1).into());
        }
    }

    let mut value = OwnedBinary::new(32).unwrap();
    value.clone_from_slice(&buffer1[..]);
    let result_tuple = (ok(), value.release(env)); 
    
    return Ok(result_tuple.encode(env))
}
